(in-package #:repl-protocol)

;;; Protocol methods shared by the Swank family (SLIME's Swank, Sly's Slynk).
;;; Both expose the same RPC set under different symbol names; a backend
;;; subclasses SWANK-FAMILY-CONNECTION and maps each operation keyword to a
;;; wire symbol with RPC-SYMBOL.

(defclass swank-family-connection (wire-connection)
  ((parsers :initform (make-hash-table) :reader connection-parsers
            :documentation "Request id → function turning its :OK value into a result.")
   (eval-counter :initform 0 :accessor connection-eval-counter))
  (:documentation "A wire connection to a Swank-like server."))

(defgeneric rpc-symbol (connection operation)
  (:documentation "Wire symbol of the server function implementing OPERATION
(a keyword such as :EVAL-AND-GRAB-OUTPUT or :RETURN-FROM-FRAME)."))

(defun quote-arg (x)
  "Swank evaluates :EMACS-REX forms, so list (and symbol) arguments are quoted."
  (if (or (consp x) (wire-symbol-p x))
      (list (wsym "COMMON-LISP" "QUOTE") x)
      x))

(defun rpc (conn operation &rest args)
  (cons (rpc-symbol conn operation) (mapcar #'quote-arg args)))

(defun cl-form (name &rest args)
  (cons (wsym "COMMON-LISP" name) args))

(defun server-form (conn name &rest args)
  "Call NAME in the server's own package (internal helpers)."
  (cons (wsym (wire-connection-rpc-package conn) name) args))

;;; Results

(defun wire-name-string (x)
  (cond ((stringp x) x)
        ((symbol-name-of x))
        (t (princ-to-string x))))

(defun parse-location (form)
  "SOURCE-LOCATION from (:location (:file f) (:position p) …) or (:error msg)."
  (cond ((and (consp form) (wire-symbol= (first form) "ERROR"))
         (make-instance 'source-location :error (second form)))
        ((and (consp form) (wire-symbol= (first form) "LOCATION"))
         (let (file position line snippet)
           (dolist (part (rest form))
             (when (consp part)
               (let ((k (symbol-name-of (first part))))
                 (cond ((equal k "FILE") (setf file (second part)))
                       ((equal k "BUFFER") (setf file (or file (second part))))
                       ((equal k "POSITION") (setf position (and (integerp (second part))
                                                                 (max 0 (1- (second part))))))
                       ((equal k "OFFSET") (setf position (and (integerp (second part))
                                                               (+ (second part) (or (third part) 0)))))
                       ((equal k "LINE") (setf line (second part)))
                       ((equal k "SNIPPET") (setf snippet (second part)))))))
           (make-instance 'source-location :file file :position position :line line :snippet snippet)))
        (t nil)))

(defun parse-compile-result (form)
  ;; (:compilation-result notes successp duration loadp faslfile)
  (destructuring-bind (&optional head notes successp duration &rest more) form
    (declare (ignore head more))
    (make-instance 'compile-result
                   :success (and successp t)
                   :duration duration
                   :notes (loop for note in notes
                                collect (make-instance
                                         'compiler-note
                                         :severity (let ((s (wire-plist-get note "SEVERITY")))
                                                     (or (and (keywordp s) s)
                                                         (wire-name-string s)))
                                         :message (wire-plist-get note "MESSAGE")
                                         :location (parse-location (wire-plist-get note "LOCATION")))))))

(defun parse-inspector (form)
  ;; (:title "…" :id n :content (parts length start end))
  (let ((content (first (wire-plist-get form "CONTENT"))))
    (make-instance 'inspector-view
                   :title (wire-plist-get form "TITLE")
                   :id (wire-plist-get form "ID")
                   :parts (loop for part in content
                                collect (cond ((stringp part)
                                               (make-instance 'inspector-part :kind :text :text part))
                                              ((wire-symbol= (first part) "VALUE")
                                               (make-instance 'inspector-part :kind :value
                                                              :text (second part) :index (third part)))
                                              ((wire-symbol= (first part) "ACTION")
                                               (make-instance 'inspector-part :kind :action
                                                              :text (second part) :index (third part)))
                                              (t (make-instance 'inspector-part :kind :text
                                                                :text (wire-name-string (second part)))))))))

(defun parse-xrefs (form)
  (when (listp form)
    (loop for entry in form
          when (consp entry)
            collect (make-instance 'xref-entry :name (wire-name-string (first entry))
                                               :location (parse-location (second entry))))))

(defun parse-eval-values (value)
  ;; our eval form returns (output (printed-value …) id)
  (destructuring-bind (output printed &optional id) value
    (make-instance 'eval-result :output output :values printed :id id)))

(defun result-from (conn status value parser)
  (ecase status
    (:ok (funcall parser value))
    (:abort (make-instance 'eval-result :aborted t :reason value))
    (:debug (let ((state value))
              (setf (gethash (debugger-state-request-id state) (connection-parsers conn)) parser)
              state))))

(defun call-with-result (conn form parser &key package (thread t) (timeout *repl-timeout*))
  (multiple-value-bind (status value) (wire-rex conn form :package package :thread thread :timeout timeout)
    (result-from conn status value parser)))

(defun debugger-thread (conn)
  (let ((state (repl-debugger-state conn)))
    (unless state (error 'repl-not-in-debugger))
    state))

(defun in-debugger (conn form &key package)
  "Run FORM in the innermost debugger's thread; signal REPL-REMOTE-ERROR on abort."
  (let ((state (debugger-thread conn)))
    (multiple-value-bind (status value)
        (wire-rex conn form :package package :thread (debugger-state-thread state))
      (case status
        (:ok value)
        (:debug value)
        (t (error 'repl-remote-error :reason value))))))

;;; Connection info

(defmethod repl-connection-info ((conn swank-family-connection))
  (let* ((plist (wire-call conn (rpc conn :connection-info)))
         (impl (wire-plist-get plist "LISP-IMPLEMENTATION")))
    (make-instance 'connection-info
                   :pid (wire-plist-get plist "PID")
                   :implementation (wire-plist-get impl "TYPE")
                   :version (wire-plist-get impl "VERSION")
                   :features (mapcar #'wire-name-string (wire-plist-get plist "FEATURES"))
                   :package (wire-plist-get (wire-plist-get plist "PACKAGE") "NAME")
                   :server-version (wire-plist-get plist "VERSION"))))

;;; Evaluation

(defun eval-form (conn string)
  "A server-side form: read STRING in the request package, evaluate it with
output captured, keep the values for later inspection, and return
(output printed-values id)."
  (let ((store (wsym (wire-connection-rpc-package conn) "*REPL-PROTOCOL-VALUES*"))
        (vals (wsym (wire-connection-rpc-package conn) "VALS"))
        (out (wsym (wire-connection-rpc-package conn) "OUT"))
        (id (incf (connection-eval-counter conn))))
    (cl-form "LET*"
             (list (list out (cl-form "MAKE-STRING-OUTPUT-STREAM"))
                   (list vals (cl-form "LET" (list (list (wsym "COMMON-LISP" "*STANDARD-OUTPUT*") out))
                                       (cl-form "MULTIPLE-VALUE-LIST"
                                                (cl-form "EVAL" (server-form conn "FROM-STRING" string))))))
             (cl-form "UNLESS" (cl-form "BOUNDP" (list (wsym "COMMON-LISP" "QUOTE") store))
                      (cl-form "SETF" (cl-form "SYMBOL-VALUE" (list (wsym "COMMON-LISP" "QUOTE") store))
                               (cl-form "MAKE-HASH-TABLE")))
             (cl-form "SETF" (cl-form "GETHASH" id (cl-form "SYMBOL-VALUE" (list (wsym "COMMON-LISP" "QUOTE") store)))
                      vals)
             (cl-form "LIST" (cl-form "GET-OUTPUT-STREAM-STRING" out)
                      (cl-form "MAPCAR" (list (wsym "COMMON-LISP" "FUNCTION") (wsym "COMMON-LISP" "PRIN1-TO-STRING"))
                               vals)
                      id))))

(defun with-buffer-syntax-form (conn form)
  (list (wsym (wire-connection-rpc-package conn) "WITH-BUFFER-SYNTAX") nil form))

(defmethod repl-eval ((conn swank-family-connection) string &key package (timeout *repl-timeout*))
  (call-with-result conn (cons (wsym "COMMON-LISP" "PROGN")
                               (list (with-buffer-syntax-form conn (eval-form conn string))))
                    #'parse-eval-values :package package :timeout timeout))

(defmethod repl-inspect-result ((conn swank-family-connection) (result eval-result) &optional (index 0))
  (repl-inspect conn (format nil "(nth ~D (gethash ~D ~A::*repl-protocol-values*))"
                             index (eval-result-id result) (wire-connection-rpc-package conn))))

(defmethod repl-compile-string ((conn swank-family-connection) string &key package filename (position 0))
  (call-with-result conn (rpc conn :compile-string string (or filename "repl") (list (list :position (1+ position)))
                              filename nil)
                    #'parse-compile-result :package package))

(defmethod repl-compile-file ((conn swank-family-connection) pathname &key load)
  (call-with-result conn (rpc conn :compile-file (namestring pathname) (and load t))
                    #'parse-compile-result))

(defmethod repl-load-system ((conn swank-family-connection) name)
  (repl-eval conn (format nil "(asdf:load-system ~S)" (string name))))

;;; Debugger

(defmethod repl-backtrace ((conn swank-family-connection) &key (start 0) (end 100))
  (mapcar #'parse-frame (in-debugger conn (rpc conn :backtrace start end))))

(defmethod repl-frame-locals ((conn swank-family-connection) frame)
  (loop for local in (first (in-debugger conn (rpc conn :frame-locals frame)))
        collect (make-instance 'frame-local :name (wire-plist-get local "NAME")
                                            :id (wire-plist-get local "ID")
                                            :value (wire-plist-get local "VALUE"))))

(defmethod repl-frame-source ((conn swank-family-connection) frame)
  (parse-location (in-debugger conn (rpc conn :frame-source-location frame))))

(defmethod repl-eval-in-frame ((conn swank-family-connection) frame string &key package)
  (in-debugger conn (rpc conn :eval-in-frame string frame (or package "COMMON-LISP-USER")) :package package))

(defun resume-debugger (conn form)
  "Send FORM in the innermost debugger's thread (it resumes the thread: a restart
or a stepper command), then collect what the request that entered the
debugger returns next: a result, or a new DEBUGGER-STATE."
  (let* ((state (debugger-thread conn))
         (request (debugger-state-request-id state))
         (parser (or (and request (gethash request (connection-parsers conn))) #'identity))
         (serial (bt2:with-lock-held ((wire-lock conn)) (wire-debug-serial conn))))
    (wire-send conn (list :emacs-rex form "COMMON-LISP-USER" (debugger-state-thread state)
                          (bt2:with-lock-held ((wire-lock conn)) (incf (wire-next-id conn)))))
    (if request
        (multiple-value-bind (status value) (wire-await conn request :serial serial)
          (unless (eq status :debug) (remhash request (connection-parsers conn)))
          (result-from conn status value parser))
        (make-instance 'eval-result :values '() :aborted nil))))

(defmethod repl-invoke-restart ((conn swank-family-connection) restart &key level)
  (let ((state (debugger-thread conn)))
    (resume-debugger conn (rpc conn :invoke-restart (or level (debugger-state-level state)) restart))))

(defmethod repl-step ((conn swank-family-connection) frame &key (mode :into))
  (resume-debugger conn (rpc conn (ecase mode (:into :step-into) (:next :step-next) (:out :step-out)) frame)))

(defmethod repl-return-from-frame ((conn swank-family-connection) frame string)
  (in-debugger conn (rpc conn :return-from-frame frame string)))

(defmethod repl-restart-frame ((conn swank-family-connection) frame)
  (in-debugger conn (rpc conn :restart-frame frame)))

;;; Inspector

(defmethod repl-inspect ((conn swank-family-connection) string &key package)
  (let ((value (wire-call conn (rpc conn :init-inspector string) :package package)))
    (if (typep value 'debugger-state) value (parse-inspector value))))

(defmethod repl-inspect-part ((conn swank-family-connection) index)
  (parse-inspector (wire-call conn (rpc conn :inspect-nth-part index))))

(defmethod repl-inspector-action ((conn swank-family-connection) index)
  (parse-inspector (wire-call conn (rpc conn :inspector-call-nth-action index))))

(defmethod repl-inspector-pop ((conn swank-family-connection))
  (let ((v (wire-call conn (rpc conn :inspector-pop)))) (and v (parse-inspector v))))

(defmethod repl-inspector-next ((conn swank-family-connection))
  (let ((v (wire-call conn (rpc conn :inspector-next)))) (and v (parse-inspector v))))

(defmethod repl-inspect-frame-var ((conn swank-family-connection) frame var)
  (parse-inspector (in-debugger conn (rpc conn :inspect-frame-var frame var))))

(defmethod repl-inspect-condition ((conn swank-family-connection))
  (parse-inspector (in-debugger conn (rpc conn :inspect-current-condition))))

;;; Introspection

(defmethod repl-describe ((conn swank-family-connection) name &key package)
  (wire-call conn (rpc conn :describe-symbol name) :package package))

(defmethod repl-documentation ((conn swank-family-connection) name &key package)
  (wire-call conn (rpc conn :documentation-symbol name) :package package))

(defmethod repl-arglist ((conn swank-family-connection) name &key package)
  (wire-call conn (rpc conn :operator-arglist name (or package "COMMON-LISP-USER")) :package package))

(defmethod repl-apropos ((conn swank-family-connection) pattern &key package external-only)
  (loop for entry in (wire-call conn (rpc conn :apropos pattern (and external-only t) nil package))
        collect (wire-plist-get entry "DESIGNATOR")))

(defmethod repl-completions ((conn swank-family-connection) prefix &key package)
  ;; Swank: ((name classification …) …); Slynk: ((name …) common-prefix).
  (let ((v (wire-call conn (rpc conn :completions prefix (or package "COMMON-LISP-USER")))))
    (if (and (consp v) (= (length v) 2) (listp (first v)) (stringp (second v)))
        (first v)
        (loop for e in v collect (if (consp e) (first e) e)))))

(defmethod repl-macroexpand ((conn swank-family-connection) string &key package all)
  (wire-call conn (rpc conn (if all :macroexpand-all :macroexpand-1) string) :package package))

(defmethod repl-xref ((conn swank-family-connection) type name &key package)
  (parse-xrefs (wire-call conn (rpc conn :xref type name) :package package)))

(defmethod repl-find-definitions ((conn swank-family-connection) name &key package)
  (parse-xrefs (wire-call conn (rpc conn :find-definitions name) :package package)))

;;; Threads

(defmethod repl-list-threads ((conn swank-family-connection))
  (let ((table (wire-call conn (rpc conn :list-threads))))
    (loop for row in (rest table)
          for i from 0
          collect (make-instance 'thread-info :index i :id (first row)
                                              :name (second row) :status (third row)))))

(defmethod repl-kill-thread ((conn swank-family-connection) index)
  (wire-call conn (rpc conn :kill-thread index)))

;;; Channels (Slynk mrepl)

(defun supported-rpc (conn operation)
  (handler-case (rpc-symbol conn operation)
    (repl-protocol-error () (error 'repl-not-supported :operation operation))))

(defun prompt-message-p (message)
  (and (consp message) (wire-symbol= (first message) "PROMPT")))

(defun error-prompt-p (message)
  ;; (:prompt package nickname error-level entry-index [condition])
  (and (prompt-message-p message) (> (length message) 5)))

(defun channel-wait (channel start &key (timeout *repl-timeout*) serial)
  "Wait for a normal prompt at or after inbox index START. → (values :prompt index),
(values :debug state) when SERIAL is given and the channel's thread entered the
debugger, or (values :timeout nil)."
  (let* ((conn (repl-channel-connection channel))
         (deadline (and timeout (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))))
    (bt2:with-lock-held ((wire-lock conn))
      (loop
        (let* ((inbox (repl-channel-inbox channel))
               (index (position-if (lambda (m) (and (prompt-message-p m) (not (error-prompt-p m))))
                                   inbox :start (min start (length inbox)))))
          (cond (index (return (values :prompt index)))
                ((and serial (/= serial (wire-debug-serial conn))
                      (let ((state (first (wire-debuggers conn))))
                        (and state (equal (debugger-state-thread state) (repl-channel-thread channel)))))
                 (return (values :debug (first (wire-debuggers conn)))))
                ((wire-closed conn)
                 (error 'repl-connection-error :message (or (wire-close-reason conn) "connection closed")))
                ((and deadline (>= (get-internal-real-time) deadline))
                 (return (values :timeout nil)))
                (t (bt2:condition-wait (wire-cv conn) (wire-lock conn)
                                       :timeout (if deadline
                                                    (max 0.01 (/ (- deadline (get-internal-real-time))
                                                                 internal-time-units-per-second))
                                                    1)))))))))

(defun drain-channel-output (channel)
  "Output from the dedicated socket, after it has been quiet for 50 ms (at most
half a second): the image flushes it before the prompt, but the socket is
read by another thread."
  (let* ((conn (repl-channel-connection channel))
         (quiet (floor internal-time-units-per-second 20))
         (give-up (+ (get-internal-real-time) (floor internal-time-units-per-second 2))))
    (bt2:with-lock-held ((wire-lock conn))
      (loop while (and (repl-channel-output-socket channel)
                       (< (- (get-internal-real-time) (repl-channel-output-at channel)) quiet)
                       (< (get-internal-real-time) give-up))
            do (bt2:condition-wait (wire-cv conn) (wire-lock conn) :timeout 0.02))
      (get-output-stream-string (repl-channel-output channel)))))

(defun channel-result (channel start end)
  "EVAL-RESULT from the inbox messages in [START, END), plus dedicated-socket output."
  (let ((output (make-string-output-stream)) (values '()) (entry nil) (aborted nil) (reason nil))
    (loop for i from start below end
          for message = (aref (repl-channel-inbox channel) i)
          for kind = (and (consp message) (symbol-name-of (first message)))
          do (alexandria:switch (kind :test #'equal)
               ("WRITE-STRING" (write-string (second message) output))
               ("WRITE-VALUES"
                ;; (presentation entry-index copy-string): numbers are presented as
                ;; "3 (2 bits, #x3, …)"; keep the plain number
                (loop for (printed index copy) in (second message)
                      for text = (if (stringp printed) printed (wire-name-string printed))
                      do (setf entry (or entry index))
                         (push (if (and (stringp copy)
                                        (> (length text) (+ (length copy) 1))
                                        (string= copy text :end2 (length copy))
                                        (string= " (" text :start2 (length copy) :end2 (+ 2 (length copy))))
                                   copy
                                   text)
                               values)))
               ("EVALUATION-ABORTED" (setf aborted t reason (second message)))))
    (write-string (drain-channel-output channel) output)
    (make-instance 'eval-result :output (get-output-stream-string output) :values (nreverse values)
                                :entry entry :aborted aborted :reason reason)))

(defmethod repl-open-channel ((conn swank-family-connection) &key package)
  (supported-rpc conn :create-mrepl)
  (let* ((remote-id (bt2:with-lock-held ((wire-lock conn)) (incf (wire-next-channel-id conn))))
         (channel (make-instance 'repl-channel :connection conn :remote-id remote-id)))
    (bt2:with-lock-held ((wire-lock conn))
      (setf (gethash remote-id (wire-channels conn)) channel))
    (let ((value (wire-call conn (rpc conn :create-mrepl remote-id))))
      (unless (and (consp value) (integerp (first value)))
        (error 'repl-remote-error :reason (format nil "could not create a channel: ~A" value)))
      (setf (repl-channel-id channel) (first value)
            (repl-channel-thread channel) (second value)))
    (multiple-value-bind (status index) (channel-wait channel 0 :timeout 30)
      (when (eq status :timeout)
        (error 'repl-timeout :seconds 30 :request :open-channel))
      (setf (repl-channel-cursor channel) (1+ index)))
    (drain-channel-output channel)          ; the banner
    (when package
      (repl-channel-eval channel (format nil "(in-package ~S)" (string package))))
    channel))

(defmethod repl-channel-eval ((channel repl-channel) string &key (timeout *repl-timeout*))
  (when (repl-channel-closed-p channel)
    (error 'repl-connection-error :message "channel is closed"))
  (let ((conn (repl-channel-connection channel)))
    ;; an evaluation that entered the debugger closes with a prompt after its restart
    (when (repl-channel-recovering channel)
      (multiple-value-bind (status index) (channel-wait channel (repl-channel-cursor channel) :timeout 5)
        (when (eq status :prompt)
          (setf (repl-channel-cursor channel) (1+ index))))
      (setf (repl-channel-recovering channel) nil))
    (let ((start (repl-channel-cursor channel))
          (serial (bt2:with-lock-held ((wire-lock conn)) (wire-debug-serial conn))))
      (wire-send conn (list :emacs-channel-send (repl-channel-id channel) (list :process string)))
      (loop
        (multiple-value-bind (status value) (channel-wait channel start :timeout timeout :serial serial)
          (ecase status
            (:prompt
             (setf (repl-channel-cursor channel) (1+ value))
             (return (channel-result channel start value)))
            (:debug
             (setf (repl-channel-recovering channel) t)
             (return value))
            (:timeout
             (restart-case (error 'repl-timeout :seconds timeout :request :channel-eval)
               (keep-waiting () :report "Keep waiting for the channel" nil)
               (interrupt ()
                 :report "Interrupt the channel's thread"
                 (wire-send conn (list :emacs-interrupt (repl-channel-thread channel))))))))))))

(defmethod repl-channel-inspect ((channel repl-channel) entry &optional (index 0))
  (let* ((conn (repl-channel-connection channel))
         ;; mrepl functions must run in the channel's own thread
         (value (multiple-value-bind (status value)
                    (wire-rex conn (rpc conn :inspect-entry (repl-channel-remote-id channel) entry index)
                              :thread (repl-channel-thread channel))
                  (ecase status
                    ((:ok :debug) value)
                    (:abort (error 'repl-remote-error :reason value))))))
    (if (typep value 'debugger-state) value (parse-inspector value))))

(defmethod repl-close-channel ((channel repl-channel))
  (let ((conn (repl-channel-connection channel)))
    (unless (repl-channel-closed-p channel)
      (setf (repl-channel-closed-p channel) t)
      (ignoring-wire-errors
       (wire-send conn (list :emacs-channel-send (repl-channel-id channel) (list :teardown))))
      (bt2:with-lock-held ((wire-lock conn))
        (remhash (repl-channel-remote-id channel) (wire-channels conn)))
      ;; shut down only: the output thread is blocked reading and closes it
      (let ((socket (repl-channel-output-socket channel)))
        (when socket
          (ignoring-wire-errors (usocket:socket-shutdown socket :io)))))
    t))
