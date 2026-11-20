(in-package #:repl-protocol/tests)

;;; KILL-IMAGE stops the spawned image's descendants too. Roswell's ros.exe on
;;; Windows runs SBCL as its child instead of exec-ing it, so killing the
;;; direct child alone left the image running (holding its working
;;; directory). Modelled here by a launcher that runs the Lisp as its child: a
;;; marker file appears a few seconds after the port unless the kill reached
;;; the Lisp.

#+sbcl
(defun %this-lisp ()
  (list (uiop:native-namestring sb-ext:*runtime-pathname*)
        "--core" (uiop:native-namestring sb-ext:*core-pathname*)
        "--noinform" "--no-sysinit" "--no-userinit"))

#+sbcl
(defun %late-marker-form (marker)
  (format nil "(progn (sleep 4) (with-open-file (o ~S :direction :output :if-exists :supersede) (write 1 :stream o)))"
          marker))

#+sbcl
(defun %launcher-and-forms (marker)
  "→ (values command extra-forms). Unix: sh runs the Lisp as its child (not
exec), and the Lisp writes MARKER late. Windows (no sh assumed): the Lisp
launches a second Lisp that writes MARKER late (job-object descendants)."
  (if (uiop:os-windows-p)
      (values repl-protocol:*default-image-command* ; no ros: this Lisp
              (list "(require :asdf)"
                    (format nil "(uiop:launch-program (list ~{~S~^ ~} \"--non-interactive\" \"--eval\" ~S) :output nil :error-output nil)"
                            (%this-lisp) (%late-marker-form marker))))
      (values (list "sh" "-c"
                    ;; IMAGE-COMMAND appends --noinform ($3): drop it, it is a runtime option
                    "r=$1; c=$2; shift 3; \"$r\" --core \"$c\" --noinform --no-sysinit --no-userinit \"$@\"; exit $?"
                    "sh" (uiop:native-namestring sb-ext:*runtime-pathname*)
                    (uiop:native-namestring sb-ext:*core-pathname*))
              '())))

#+sbcl
(deftest kill-image-reaches-descendants
  (let* ((repl-protocol:*default-image-command* '("no-such-ros-5a1c" "run" "--"))
         (process-protocol:*process-backend* (or process-protocol:*process-backend*
                                                 (process-backend-uiop:use-uiop-process-backend)))
         (marker (uiop:native-namestring
                  (merge-pathnames (format nil "repl-descendant-~36R.mark"
                                           (random (expt 36 8) (make-random-state t)))
                                   (uiop:temporary-directory)))))
    (unwind-protect
         (multiple-value-bind (command extra) (%launcher-and-forms marker)
           (multiple-value-bind (port process)
               (repl-protocol:spawn-image
                (lambda (port-file)
                  (append extra
                          (list (format nil "(with-open-file (o ~S :direction :output :if-exists :supersede) (write 4244 :stream o))" port-file)
                                (%late-marker-form marker)
                                "(sleep 600)")))
                :command command :timeout 60)
             (ok (= port 4244))
             (ok (null (repl-protocol:kill-image process)))
             (process-protocol:wait process :timeout 20)
             (ok (not (process-protocol:alive-p process)) "the launcher exited")
             (sleep 6)
             (ok (not (probe-file marker)) "the Lisp under the launcher was killed with it")))
      (uiop:delete-file-if-exists marker))))
