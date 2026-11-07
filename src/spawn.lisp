(in-package #:repl-protocol)

;;; Spawning an image: run COMMAND with --eval forms that load a server and
;;; write its port to a file, then wait for the file. Needs a PROCESS-PROTOCOL
;;; backend (e.g. process-backend-uiop) to be active.

(defparameter *default-image-command* '("ros" "run" "--")
  "Command prefix for a fresh image; arguments after it go to SBCL.
Roswell gives the child Quicklisp, so servers load with QL:QUICKLOAD.")

(defun find-program (name)
  "Full path of program NAME found on PATH, or NAME itself when it has a
directory part or is not found; the second value is true when it was found.
On Windows the .exe/.cmd/.bat/.com extensions are tried too: CreateProcess
does not search for them."
  (if (or (find #\/ name) (find #\\ name))
      (values name (and (uiop:file-exists-p name) t))
      (let* ((windows (uiop:os-windows-p))
             (separator (if windows #\; #\:))
             (extensions (if (and windows (not (pathname-type name)))
                             '("" ".exe" ".cmd" ".bat" ".com")
                             '(""))))
        (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "") :separator (list separator))
              when (plusp (length dir))
                do (loop for ext in extensions
                         for candidate = (uiop:merge-pathnames*
                                          (uiop:parse-native-namestring (concatenate 'string name ext))
                                          (uiop:ensure-directory-pathname (uiop:parse-native-namestring dir)))
                         when (uiop:file-exists-p candidate)
                           do (return-from find-program (values (uiop:native-namestring candidate) t))))
        (values name nil))))

(defun current-lisp-command ()
  "Command that starts this Lisp again (SBCL: its runtime and core, without
init files), and --eval forms that load Quicklisp in the child when this image
has it. NIL when the implementation is not supported."
  #+sbcl
  (values (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                "--core" (uiop:native-namestring sb-ext:*core-pathname*)
                "--noinform" "--no-sysinit" "--no-userinit")
          (let ((ql (find-package "QL")))
            (when ql
              (let ((home (symbol-value (find-symbol "*QUICKLISP-HOME*" ql))))
                ;; Roswell's Quicklisp has local-init files that need Roswell itself:
                ;; skip them through Quicklisp's own SKIP restart
                (list (format nil "(handler-bind ((error (lambda (c) (let ((r (find \"SKIP\" (compute-restarts c) :key (lambda (r) (string (restart-name r))) :test (function string=)))) (when r (invoke-restart r)))))) (load ~S))"
                              (uiop:native-namestring (merge-pathnames "setup.lisp" home))))))))
  #-sbcl nil)

(defun eval-arguments (forms)
  (loop for f in forms append (list "--eval" f)))

(defun image-command (command forms)
  "Argument list for COMMAND running FORMS. When COMMAND is the default (Roswell)
and Roswell is not on PATH, the image is this Lisp's own runtime instead."
  (multiple-value-bind (program found) (find-program (first command))
    (multiple-value-bind (prefix setup)
        (and (not found) (equal command *default-image-command*) (current-lisp-command))
      (if prefix
          (append prefix (eval-arguments (append setup forms)))
          (append (cons program (rest command)) (list "--noinform") (eval-arguments forms))))))

(defun file-tail (path &optional (max 4000))
  (when (and path (probe-file path))
    (let ((text (handler-case (alexandria:read-file-into-string path :external-format :utf-8)
                  (stream-error () ""))))
      (subseq text (max 0 (- (length text) max))))))

(defun read-port-file (path)
  (when (probe-file path)
    (let ((text (handler-case (alexandria:read-file-into-string path)
                  (stream-error () ""))))
      (parse-integer text :junk-allowed t))))

(defun spawn-image (forms-function &key (command *default-image-command*) directory env (timeout 180))
  "Launch an image. FORMS-FUNCTION gets the port-file namestring and returns the
--eval forms (strings) that start a server writing its port there.
→ (values port process-handle log-path)."
  (let* ((stamp (format nil "~36R" (random (expt 36 8) (make-random-state t))))
         (dir (uiop:temporary-directory))
         (port-file (merge-pathnames (format nil "repl-~A.port" stamp) dir))
         (log-file (merge-pathnames (format nil "repl-~A.log" stamp) dir))
         (cmd (image-command command (funcall forms-function (namestring port-file))))
         (process (handler-case
                      (process-protocol:launch cmd :output log-file :error :output
                                                   :directory directory :env env)
                    ;; SBCL's RUN-PROGRAM signals a SIMPLE-ERROR for a missing program
                    ((or process-protocol:process-error file-error simple-error) (e)
                      (error 'repl-spawn-error :message (format nil "cannot run ~{~A~^ ~}" command)
                                               :cause e)))))
    (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
      (loop
        (let ((port (read-port-file port-file)))
          (when port
            (uiop:delete-file-if-exists port-file)
            (return (values port process log-file))))
        (unless (process-protocol:alive-p process)
          (error 'repl-spawn-error :message "the image exited before starting its server"
                                   :log (file-tail log-file)))
        (when (> (get-internal-real-time) deadline)
          (process-protocol:kill process :force t)
          (error 'repl-spawn-error :message (format nil "no server port after ~D s" timeout)
                                   :log (file-tail log-file)))
        (sleep 0.1)))))
