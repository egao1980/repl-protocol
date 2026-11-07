(in-package #:repl-protocol)

;;; A Swank-family wire connection: one socket, one reader thread that
;;; dispatches :RETURN by continuation id and queues everything else.
;;; Backends (swank, slynk) subclass it and map protocol GFs to RPCs.

(defclass wire-connection (repl-connection)
  ((socket :initarg :socket :reader wire-socket)
   (host :initarg :host :initform "127.0.0.1" :reader wire-host)
   (stream :initarg :stream :reader wire-stream)
   (rpc-package :initarg :rpc-package :reader wire-connection-rpc-package
                :documentation "Package of the server's RPC functions, e.g. \"SWANK\".")
   (lock :initform (bt2:make-lock :name "repl wire") :reader wire-lock)
   (send-lock :initform (bt2:make-lock :name "repl wire send") :reader wire-send-lock)
   (cv :initform (bt2:make-condition-variable) :reader wire-cv)
   (next-id :initform 0 :accessor wire-next-id)
   (pending :initform (make-hash-table) :reader wire-pending
            :documentation "Continuation id → (:waiting) / (:ok value) / (:abort reason).")
   (debuggers :initform '() :accessor wire-debuggers
              :documentation "Active DEBUGGER-STATEs, innermost first.")
   (debug-serial :initform 0 :accessor wire-debug-serial)
   (output :initform (make-string-output-stream) :accessor wire-output)
   (events :initform '() :accessor wire-events)
   (indentation :initform '() :accessor wire-indentation)
   (channels :initform (make-hash-table) :reader wire-channels
             :documentation "Remote id → REPL-CHANNEL.")
   (next-channel-id :initform 0 :accessor wire-next-channel-id)
   (event-listener :initform nil :initarg :event-listener :accessor wire-event-listener)
   (closed :initform nil :accessor wire-closed)
   (released :initform nil :accessor wire-released
             :documentation "True once the socket has been shut down and closed.")
   (close-reason :initform nil :accessor wire-close-reason)
   (reader :initform nil :accessor wire-reader-thread)
   (last-thread :initform t :accessor wire-last-thread
                :documentation "Thread designator of the last request's debugger, for interrupts.")
   (read-string-function :initarg :read-string-function :initform (constantly "")
                         :accessor wire-read-string-function
                         :documentation "Called with (thread tag) when the image reads *standard-input*.")))

(defun open-wire-connection (class host port &rest initargs)
  "Connect to HOST:PORT and start the reader thread. → instance of CLASS."
  (let ((socket (handler-case
                    (usocket:socket-connect host port :element-type '(unsigned-byte 8))
                  (usocket:socket-error (e)
                    (error 'repl-connection-error :message (format nil "cannot connect to ~A:~D" host port)
                                                  :cause e)))))
    (let ((conn (apply #'make-instance class :socket socket :host host
                                             :stream (usocket:socket-stream socket) initargs)))
      (setf (wire-reader-thread conn)
            (bt2:make-thread (lambda () (reader-loop conn))
                             :name (format nil "repl wire reader ~A:~D" host port)))
      conn)))

(defun wire-send (conn form)
  (when (wire-closed conn)
    (error 'repl-connection-error :message (or (wire-close-reason conn) "connection is closed")))
  (bt2:with-lock-held ((wire-send-lock conn))
    (handler-case (write-message (wire-stream conn) form)
      (stream-error (e)
        (error 'repl-connection-error :message "write failed" :cause e)))))

(defmacro ignoring-wire-errors (&body body)
  "Run BODY; a broken connection there is not the caller's problem."
  `(handler-case (progn ,@body)
     ((or repl-error stream-error usocket:socket-error
          ;; usocket does not wrap shutdown(2) failures on every platform
          #+sbcl sb-bsd-sockets:socket-error)
         () nil)))

(defun wire-close (conn)
  (unless (wire-closed conn)
    (bt2:with-lock-held ((wire-lock conn))
      (setf (wire-closed conn) t
            (wire-close-reason conn) (or (wire-close-reason conn) "closed by client"))
      (bt2:condition-broadcast (wire-cv conn))))
  ;; Shut down first so the reader thread's blocking read returns, join it,
  ;; and only then free the descriptor: closing an fd another thread is
  ;; selecting on can corrupt SBCL's fd handling. Idempotent.
  (unless (bt2:with-lock-held ((wire-lock conn))
            (shiftf (wire-released conn) t))
    (ignoring-wire-errors (usocket:socket-shutdown (wire-socket conn) :io))
    (dolist (channel (bt2:with-lock-held ((wire-lock conn))
                       (alexandria:hash-table-values (wire-channels conn))))
      (setf (repl-channel-closed-p channel) t)
      (let ((socket (repl-channel-output-socket channel)))
        (when socket (ignoring-wire-errors (usocket:socket-shutdown socket :io)))))
    (let ((reader (wire-reader-thread conn)))
      (when (and reader (not (eq reader (bt2:current-thread))))
        (loop repeat 200
              while (bt2:thread-alive-p reader)
              do (sleep 0.01))))
    (ignoring-wire-errors (usocket:socket-close (wire-socket conn))))
  conn)

;;; Reader thread

(defun reader-loop (conn)
  ;; Thread boundary: any failure here (closed socket, bad frame, an SBCL
  ;; fd error after WIRE-CLOSE) ends the connection and is reported to
  ;; waiters through CLOSE-REASON instead of killing the thread.
  (handler-case
      (loop (dispatch-event conn (read-message (wire-stream conn))))
    (error (e)
      (bt2:with-lock-held ((wire-lock conn))
        (setf (wire-closed conn) t
              (wire-close-reason conn) (or (wire-close-reason conn) (princ-to-string e)))
        (bt2:condition-broadcast (wire-cv conn))))))

(defun event-head (form)
  (and (consp form) (symbol-name-of (first form))))

(defgeneric parse-debugger-event (conn form)
  (:documentation "DEBUGGER-STATE from a (:debug thread level condition restarts frames conts) event."))

(defmethod parse-debugger-event ((conn wire-connection) form)
  (destructuring-bind (thread level condition restarts frames &rest more) (rest form)
    (declare (ignore more))
    (make-instance 'debugger-state
                   :thread thread :level level
                   :condition (if (consp condition) (first condition) condition)
                   :condition-type (and (consp condition) (second condition))
                   :restarts (loop for (name description) in restarts
                                   for i from 0
                                   collect (make-instance 'restart-info :index i :name name
                                                                        :description description))
                   :frames (mapcar #'parse-frame frames))))

(defun parse-frame (frame)
  (destructuring-bind (index description &optional plist) frame
    (make-instance 'frame-info :index index :description description
                               :restartable (and (listp plist) (wire-plist-get plist "RESTARTABLE")))))

(defun open-channel-output (conn channel port)
  "The image wants a dedicated output socket for CHANNEL on PORT (Slynk mrepl):
connect and copy its text into the channel's output buffer."
  (bt2:make-thread
   (lambda ()
     (let ((socket (handler-case (usocket:socket-connect (wire-host conn) port :element-type 'character)
                     (usocket:socket-error () nil))))
       (when socket
         (setf (repl-channel-output-socket channel) socket)
         (let ((stream (usocket:socket-stream socket)))
           ;; thread boundary: the socket is shut down when the channel closes
           (handler-case
               (loop for c = (read-char stream nil nil)
                     while c
                     do (bt2:with-lock-held ((wire-lock conn))
                          (write-char c (repl-channel-output channel))
                          (setf (repl-channel-output-at channel) (get-internal-real-time))
                          (bt2:condition-broadcast (wire-cv conn))))
             (error () nil))
           (ignoring-wire-errors (usocket:socket-close socket))))))
   :name (format nil "repl channel ~A output" (repl-channel-remote-id channel))))

(defun notify-listener (conn kind data)
  "Call the connection's event listener. Its errors become warnings: they must
not end the reader thread."
  (let ((fn (wire-event-listener conn)))
    (when fn
      (handler-case (funcall fn kind data)
        (error (e) (warn "REPL event listener failed on ~S: ~A" kind e))))))

(defun dispatch-event (conn form)
  (let ((head (event-head form))
        (notice nil))
    (bt2:with-lock-held ((wire-lock conn))
      (alexandria:switch (head :test #'equal)
        ("RETURN"
         (destructuring-bind (value id) (rest form)
           (setf (gethash id (wire-pending conn))
                 (if (and (consp value) (wire-symbol= (first value) "OK"))
                     (list :ok (second value))
                     (list :abort (and (consp value) (second value)))))))
        ("DEBUG"
         (let ((state (parse-debugger-event conn form)))
           (setf (wire-debuggers conn)
                 (cons state (remove-if (lambda (d) (and (equal (debugger-state-thread d) (debugger-state-thread state))
                                                         (>= (debugger-state-level d) (debugger-state-level state))))
                                        (wire-debuggers conn))))
           (setf (wire-last-thread conn) (debugger-state-thread state))
           (incf (wire-debug-serial conn))
           (setf notice (list :debug state))))
        ("DEBUG-RETURN"
         (destructuring-bind (thread level &rest more) (rest form)
           (declare (ignore more))
           (setf notice (list :debug-return (list thread level)))
           (setf (wire-debuggers conn)
                 (remove-if (lambda (d) (and (equal (debugger-state-thread d) thread)
                                             (>= (debugger-state-level d) level)))
                            (wire-debuggers conn)))))
        ("DEBUG-ACTIVATE" nil)
        ("WRITE-STRING"
         (write-string (second form) (wire-output conn))
         (setf notice (list :output (second form))))
        ("PING"
         (destructuring-bind (thread tag) (rest form)
           (bt2:make-thread (lambda () (ignoring-wire-errors
                                        (wire-send conn (list :emacs-pong thread tag))))
                            :name "repl pong")))
        ("READ-STRING"
         (destructuring-bind (thread tag) (rest form)
           (let ((fn (wire-read-string-function conn)))
             (bt2:make-thread (lambda ()
                                (ignoring-wire-errors
                                 (wire-send conn (list :emacs-return-string thread tag
                                                       (funcall fn thread tag)))))
                              :name "repl read-string"))))
        ("INDENTATION-UPDATE"
         (setf (wire-indentation conn)
               (append (second form) (wire-indentation conn)))
         (setf notice (list :indentation (second form))))
        ("CHANNEL-SEND"
         (destructuring-bind (remote-id message) (rest form)
           (let ((channel (gethash remote-id (wire-channels conn))))
             (if channel
                 (progn
                   (when (and (consp message) (wire-symbol= (first message) "PROMPT"))
                     (setf (repl-channel-package channel) (second message)))
                   (when (and (consp message) (wire-symbol= (first message) "OPEN-DEDICATED-OUTPUT-STREAM"))
                     (open-channel-output conn channel (second message)))
                   (vector-push-extend message (repl-channel-inbox channel))
                   (setf notice (list :channel (list channel message))))
                 (push (make-instance 'repl-event :kind head :data (rest form)) (wire-events conn))))))
        ("INVALID-RPC"
         (destructuring-bind (id message) (rest form)
           (setf (gethash id (wire-pending conn)) (list :abort message))))
        (t (push (make-instance 'repl-event :kind head :data (rest form)) (wire-events conn))))
      (bt2:condition-broadcast (wire-cv conn)))
    (when notice
      (notify-listener conn (first notice) (second notice)))))

;;; Requests

(defun wire-latest-debugger (conn)
  (bt2:with-lock-held ((wire-lock conn))
    (first (wire-debuggers conn))))

(defun wire-rex (conn form &key package (thread t) (timeout *repl-timeout*))
  "Send (:emacs-rex FORM PACKAGE THREAD id) and wait.
→ (values status value): :OK value, :ABORT reason, or :DEBUG DEBUGGER-STATE when
the image entered the debugger before replying. The request then stays pending
and its reply is collected by WIRE-AWAIT after a restart."
  (let ((id (bt2:with-lock-held ((wire-lock conn))
              (let ((id (incf (wire-next-id conn))))
                (setf (gethash id (wire-pending conn)) (list :waiting))
                id))))
    (wire-send conn (list :emacs-rex form (or package "COMMON-LISP-USER") thread id))
    (wire-await conn id :timeout timeout)))

(defun wire-await (conn id &key (timeout *repl-timeout*) (serial nil))
  "Wait for the reply to request ID, or for a new debugger level."
  (let ((deadline (and timeout (+ (get-internal-real-time)
                                  (* timeout internal-time-units-per-second))))
        (serial (or serial (bt2:with-lock-held ((wire-lock conn)) (wire-debug-serial conn)))))
    (loop
      (multiple-value-bind (status value)
          (bt2:with-lock-held ((wire-lock conn))
            (loop
              (let ((cell (gethash id (wire-pending conn))))
                (cond ((not (eq (first cell) :waiting))
                       (remhash id (wire-pending conn))
                       (return (values (first cell) (second cell))))
                      ((/= serial (wire-debug-serial conn))
                       (let ((state (first (wire-debuggers conn))))
                         (if state
                             (progn (setf (debugger-state-request-id state) id)
                                    (return (values :debug state)))
                             ;; the level opened and closed before we woke: keep waiting
                             (setf serial (wire-debug-serial conn)))))
                      ((wire-closed conn)
                       (return (values :closed (wire-close-reason conn))))
                      ((and deadline (>= (get-internal-real-time) deadline))
                       (return (values :timeout nil)))
                      (t (bt2:condition-wait (wire-cv conn) (wire-lock conn)
                                             :timeout (if deadline
                                                          (max 0.01 (/ (- deadline (get-internal-real-time))
                                                                       internal-time-units-per-second))
                                                          1)))))))
        (case status
          (:closed (error 'repl-connection-error :message (or value "connection closed")))
          (:timeout
           (restart-case (error 'repl-timeout :seconds timeout :request id)
             (keep-waiting ()
               :report "Keep waiting for the reply"
               (setf deadline (and timeout (+ (get-internal-real-time)
                                              (* timeout internal-time-units-per-second)))))
             (interrupt ()
               :report "Interrupt the request in the image"
               (wire-send conn (list :emacs-interrupt t))
               (setf deadline (and timeout (+ (get-internal-real-time)
                                              (* timeout internal-time-units-per-second)))))))
          (t (return (values status value))))))))

(defun wire-call (conn form &key package (timeout *repl-timeout*))
  "WIRE-REX that expects a plain value: :ABORT signals REPL-REMOTE-ERROR,
a debugger entry returns the DEBUGGER-STATE as the value."
  (multiple-value-bind (status value) (wire-rex conn form :package package :timeout timeout)
    (ecase status
      (:ok value)
      (:debug value)
      (:abort (error 'repl-remote-error :reason value)))))

;;; Generic protocol methods shared by Swank-family backends

(defmethod repl-connected-p ((conn wire-connection))
  (not (wire-closed conn)))

(defmethod repl-debugger-state ((conn wire-connection))
  (wire-latest-debugger conn))

(defmethod repl-output ((conn wire-connection) &key clear)
  (bt2:with-lock-held ((wire-lock conn))
    (let ((text (get-output-stream-string (wire-output conn))))
      (unless clear (write-string text (wire-output conn)))
      text)))

(defmethod repl-events ((conn wire-connection) &key clear)
  (bt2:with-lock-held ((wire-lock conn))
    (prog1 (reverse (wire-events conn))
      (when clear (setf (wire-events conn) '())))))

(defmethod repl-event-listener ((conn wire-connection))
  (wire-event-listener conn))

(defmethod (setf repl-event-listener) (function (conn wire-connection))
  (setf (wire-event-listener conn) function))

(defmethod repl-indentation ((conn wire-connection))
  (bt2:with-lock-held ((wire-lock conn))
    (copy-list (wire-indentation conn))))

(defmethod repl-interrupt ((conn wire-connection) &key thread)
  ;; T asks the server for its most recent worker thread.
  (wire-send conn (list :emacs-interrupt (or thread t)))
  t)

(defmethod repl-disconnect ((conn wire-connection) &key kill)
  (wire-close conn)
  (when (and kill (connection-process conn))
    (handler-case (process-protocol:kill (connection-process conn) :force t)
      (process-protocol:process-error () nil)))
  conn)
