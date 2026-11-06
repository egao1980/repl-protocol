(in-package #:repl-protocol)

;;; Spawning an image: run COMMAND with --eval forms that load a server and
;;; write its port to a file, then wait for the file. Needs a PROCESS-PROTOCOL
;;; backend (e.g. process-backend-uiop) to be active.

(defparameter *default-image-command* '("ros" "run" "--")
  "Command prefix for a fresh image; arguments after it go to SBCL.
Roswell gives the child Quicklisp, so servers load with QL:QUICKLOAD.")

(defun image-command (command forms)
  (append command
          (list "--noinform")
          (loop for f in forms append (list "--eval" f))))

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
                    ((or process-protocol:process-error file-error) (e)
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
