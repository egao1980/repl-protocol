(defpackage #:repl-protocol/conformance
  (:use #:cl #:rove #:repl-protocol)
  (:export #:run-conformance))

(in-package #:repl-protocol/conformance)

;;; Shared checks for a backend connected to a live image. Call
;;; RUN-CONFORMANCE inside a Rove DEFTEST of the backend's own test system.

(defun restart-index (state name)
  (let ((r (find-if (lambda (r) (search name (string (restart-info-name r)) :test #'char-equal))
                    (debugger-state-restarts state))))
    (and r (restart-info-index r))))

(defun abort-debugger (conn)
  (loop for state = (repl-debugger-state conn)
        while state
        do (handler-case (repl-invoke-restart conn (or (restart-index state "ABORT") 0))
             ;; the level returned between the check and the restart
             (repl-not-in-debugger () (return)))))

(defun run-conformance (conn)
  (abort-debugger conn)
  (testing "connection info"
    (let ((info (repl-connection-info conn)))
      (ok (stringp (connection-info-implementation info)))
      (ok (integerp (connection-info-pid info)))
      (ok (member "COMMON-LISP" (connection-info-features info) :test #'string-equal))))

  (abort-debugger conn)
  (testing "eval values, output, package"
    (ok (equal (eval-result-values (repl-eval conn "(+ 1 2)")) '("3")))
    (ok (equal (eval-result-values (repl-eval conn "(values 1 :a \"s\")")) '("1" ":A" "\"s\"")))
    (let ((r (repl-eval conn "(progn (princ \"hi\") 42)")))
      (ok (equal (eval-result-output r) "hi"))
      (ok (equal (eval-result-values r) '("42"))))
    (ok (equal (eval-result-values (repl-eval conn "(cl:package-name cl:*package*)" :package "KEYWORD"))
               '("\"KEYWORD\""))))

  (abort-debugger conn)
  (testing "error enters the debugger as a value"
    (let ((state (repl-eval conn "(error \"boom ~A\" 1)")))
      (ok (typep state 'debugger-state))
      (ok (search "boom 1" (debugger-state-condition state)))
      (ok (restart-index state "ABORT"))
      (ok (plusp (length (repl-backtrace conn :start 0 :end 5))))
      (ok (listp (repl-frame-locals conn 0)))
      (ok (typep (repl-inspect-condition conn) 'inspector-view))
      (let ((r (repl-invoke-restart conn (restart-index state "ABORT"))))
        (ok (typep r 'eval-result))
        (ok (eval-result-aborted-p r)))
      (ok (null (repl-debugger-state conn)))))

  (abort-debugger conn)
  (testing "a restart can return a value"
    (let ((state (repl-eval conn "(restart-case (error \"x\") (use-forty-two () 42))")))
      (ok (typep state 'debugger-state))
      (let ((r (repl-invoke-restart conn (restart-index state "USE-FORTY-TWO"))))
        (ok (equal (eval-result-values r) '("42"))))
      (ok (null (repl-debugger-state conn)))))

  (abort-debugger conn)
  (testing "nested debugger levels"
    (let* ((outer (repl-eval conn "(error \"outer\")"))
           (inner (repl-eval-in-frame conn 0 "(error \"inner\")")))
      (ok (typep inner 'debugger-state))
      (ok (search "inner" (debugger-state-condition inner)) "the frame eval itself raised")
      (ok (> (debugger-state-level inner) (debugger-state-level outer)))
      (abort-debugger conn)
      (ok (null (repl-debugger-state conn)))))

  (abort-debugger conn)
  (testing "inspector"
    (let* ((r (repl-eval conn "(list 1 2 3)"))
           (view (repl-inspect-result conn r)))
      (ok (typep view 'inspector-view))
      (ok (search "CONS" (inspector-view-title view) :test #'char-equal))
      (let ((part (find :value (inspector-view-parts view) :key #'inspector-part-kind)))
        (ok part)
        (ok (typep (repl-inspect-part conn (inspector-part-index part)) 'inspector-view))
        (ok (typep (repl-inspector-pop conn) 'inspector-view)))))

  (abort-debugger conn)
  (testing "compiler notes and introspection"
    (let ((c (repl-compile-string conn "(defun conformance-f (x) (+ x conformance-undefined-var))")))
      (ok (typep c 'compile-result))
      (ok (some (lambda (n) (search "CONFORMANCE-UNDEFINED-VAR" (compiler-note-message n) :test #'char-equal))
                (compile-result-notes c))))
    (repl-compile-string conn "(defun conformance-caller () (conformance-f 1))")
    (ok (listp (repl-xref conn :calls "conformance-f")))
    (ok (search "CAR" (repl-describe conn "car") :test #'char-equal))
    (ok (search "FUNCTION" (repl-arglist conn "mapcar") :test #'char-equal))
    (ok (member "mapcar" (repl-completions conn "mapc") :test #'string-equal))
    (ok (search "IF" (repl-macroexpand conn "(when a b)") :test #'char-equal))
    (ok (plusp (length (repl-find-definitions conn "conformance-f"))))
    (ok (plusp (length (repl-list-threads conn)))))

  (abort-debugger conn)
  (testing "interrupt a running evaluation"
    (let* ((result nil)
           (worker (bt2:make-thread (lambda () (setf result (repl-eval conn "(loop (sleep 0.01))" :timeout 30)))
                                    :name "conformance eval")))
      (sleep 1)
      (repl-interrupt conn)
      (bt2:join-thread worker)
      (ok (typep result 'debugger-state))
      (abort-debugger conn)))

  (abort-debugger conn)
  (testing "timeout is a condition with restarts"
    (ok (signals (repl-eval conn "(sleep 3)" :timeout 0.3) 'repl-timeout)))

  (abort-debugger conn)
  (testing "event listener sees debugger entries and output"
    (let ((seen '()))
      (setf (repl-event-listener conn) (lambda (kind data) (declare (ignore data)) (push kind seen)))
      (unwind-protect
           (progn
             (repl-eval conn "(error \"listened\")")
             (abort-debugger conn)
             (ok (member :debug seen))
             (ok (member :debug-return seen)))
        (setf (repl-event-listener conn) nil))))

  (abort-debugger conn)
  (testing "stepping"
    (repl-eval conn "(defun conformance-step (x) (declare (optimize (debug 3) (speed 0))) (let ((y (1+ x))) (* y 2)))")
    (let ((state (repl-eval conn "(step (conformance-step 1))")))
      (ok (typep state 'debugger-state) "STEP enters the stepper")
      (let ((result (loop repeat 20
                          for r = (repl-step conn 0 :mode :out)
                          unless (typep r 'debugger-state) return r)))
        (ok (typep result 'eval-result) "stepping out finishes the evaluation")
        (ok (equal (and result (eval-result-values result)) '("4"))))))

  (abort-debugger conn)
  (let ((channel (handler-case (repl-open-channel conn)
                   (repl-not-supported () nil))))
    (when channel
      (testing "channels: separate REPLs with value references"
        (unwind-protect
             (let ((other (repl-open-channel conn :package "KEYWORD")))
               (unwind-protect
                    (progn
                      (let ((r (repl-channel-eval channel "(list 1 2 3)")))
                        (ok (equal (eval-result-values r) '("(1 2 3)")))
                        (ok (integerp (eval-result-entry r)))
                        (let ((r2 (repl-channel-eval channel
                                                     (format nil "(length #v~D:0)" (eval-result-entry r)))))
                          (ok (equal (eval-result-values r2) '("3"))))
                        (ok (typep (repl-channel-inspect channel (eval-result-entry r)) 'inspector-view)))
                      (ok (equal (eval-result-output (repl-channel-eval channel "(princ \"out\")")) "out"))
                      (ok (equal (repl-channel-package other) "KEYWORD"))
                      (ok (not (equal (repl-channel-thread channel) (repl-channel-thread other))))
                      (let ((state (repl-channel-eval channel "(error \"in channel\")")))
                        (ok (typep state 'debugger-state))
                        (abort-debugger conn)
                        (ok (equal (eval-result-values (repl-channel-eval channel "(+ 1 1)")) '("2")))))
                 (repl-close-channel other)))
          (repl-close-channel channel))
        (ok (repl-channel-closed-p channel))))))
