(in-package #:repl-protocol/tests)

;;; A scripted in-process server speaking the Swank framing.

(defun fake-server-loop (stream)
  (let ((pending-debug nil))
    (loop
      (let* ((msg (handler-case (read-message stream)
                    (repl-error () (return))))
             (head (symbol-name (first msg))))
        (flet ((send (form) (write-message stream form)))
          (cond
            ((string= head "EMACS-REX")
             (destructuring-bind (form package thread id) (rest msg)
               (declare (ignore package thread))
               (let ((op (repl-protocol::symbol-name-of (first form))))
                 (cond ((string= op "ECHO") (send (list :return (list :ok (second form)) id)))
                       ((string= op "OUT")
                        (send (list :write-string "hello "))
                        (send (list :return (list :ok "done") id)))
                       ((string= op "FAIL")
                        (setf pending-debug id)
                        (send (list :debug 1 1 (list "boom" "SIMPLE-ERROR")
                                    (list (list "ABORT" "Return to top level")
                                          (list "USE-VALUE" "Use a value"))
                                    (list (list 0 "(FOO)" (list :restartable t)) (list 1 "(BAR)"))
                                    nil))
                        (send (list :debug-activate 1 1 nil)))
                       ((string= op "RESTART")
                        (send (list :debug-return 1 1 nil))
                        (send (list :return (list :abort "restarted") pending-debug))
                        (send (list :return (list :ok nil) id)))
                       ((string= op "PING")
                        (send (list :ping 1 7))
                        (let ((pong (read-message stream)))
                          (send (list :return (list :ok (repl-protocol::symbol-name-of (first pong))) id))))
                       ((string= op "SLOW") nil)
                       (t (send (list :return (list :abort "unknown") id)))))))
            ((string= head "EMACS-INTERRUPT") nil)))))))

(defmacro with-fake-server ((conn) &body body)
  (let ((listener (gensym "LISTENER")) (thread (gensym "THREAD")))
    `(let* ((,listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                            :element-type '(unsigned-byte 8)))
            (,thread (bt2:make-thread
                      (lambda ()
                        (let ((client (usocket:socket-accept ,listener)))
                          (unwind-protect (fake-server-loop (usocket:socket-stream client))
                            (usocket:socket-close client))))
                      :name "fake swank"))
            (,conn (open-wire-connection 'wire-connection "127.0.0.1"
                                         (usocket:get-local-port ,listener)
                                         :rpc-package "SWANK")))
       (unwind-protect (progn ,@body)
         (wire-close ,conn)
         (usocket:socket-close ,listener)
         (bt2:join-thread ,thread)))))

(deftest rex-returns-values
  (with-fake-server (conn)
    (ok (equal (multiple-value-list (wire-rex conn (list (wsym "T" "ECHO") "hi"))) '(:ok "hi")))
    (ok (equal (multiple-value-list (wire-rex conn (list (wsym "T" "NOPE")))) '(:abort "unknown")))
    (ok (signals (wire-call conn (list (wsym "T" "NOPE"))) 'repl-remote-error))))

(deftest debugger-entry-is-a-value
  (with-fake-server (conn)
    (multiple-value-bind (status state) (wire-rex conn (list (wsym "T" "FAIL")))
      (ok (eq status :debug))
      (ok (typep state 'debugger-state))
      (ok (equal (debugger-state-condition state) "boom"))
      (ok (= 2 (length (debugger-state-restarts state))))
      (ok (frame-info-restartable-p (first (debugger-state-frames state))))
      (ok (eq (repl-debugger-state conn) state))
      ;; the interrupted request completes after a restart
      (let ((request (debugger-state-request-id state)))
        (ok (equal (multiple-value-list (wire-rex conn (list (wsym "T" "RESTART")))) '(:ok nil)))
        (ok (equal (multiple-value-list (repl-protocol::wire-await conn request)) '(:abort "restarted")))
        (ok (null (repl-debugger-state conn)))))))

(deftest output-and-ping
  (with-fake-server (conn)
    (ok (equal (wire-call conn (list (wsym "T" "OUT"))) "done"))
    (ok (string= (repl-output conn :clear t) "hello "))
    (ok (string= (repl-output conn) ""))
    (ok (equal (wire-call conn (list (wsym "T" "PING"))) "EMACS-PONG"))))

(deftest timeout-offers-restarts
  (with-fake-server (conn)
    (ok (signals (wire-rex conn (list (wsym "T" "SLOW")) :timeout 0.2) 'repl-timeout))
    (let ((attempts 0))
      (ok (signals (handler-bind ((repl-timeout (lambda (c)
                                                  (declare (ignore c))
                                                  (when (< (incf attempts) 3)
                                                    (invoke-restart 'keep-waiting)))))
                     (wire-rex conn (list (wsym "T" "SLOW")) :timeout 0.1))
                   'repl-timeout))
      (ok (= attempts 3)))))

(deftest closed-connection-signals
  (with-fake-server (conn)
    (wire-close conn)
    (ok (not (repl-connected-p conn)))
    (ok (signals (wire-rex conn (list (wsym "T" "ECHO") 1)) 'repl-connection-error))))

(deftest close-is-idempotent
  (with-fake-server (conn)
    (wire-close conn)
    (ok (wire-close conn))
    (ok (wire-close conn))))
