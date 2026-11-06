(in-package #:repl-protocol)

(define-condition repl-error (error) ()
  (:documentation "Root of repl-protocol errors."))

(define-condition repl-connection-error (repl-error)
  ((cause :initarg :cause :initform nil :reader repl-connection-error-cause)
   (message :initarg :message :initform "connection failed" :reader repl-connection-error-message))
  (:report (lambda (c s)
             (format s "REPL connection error: ~A~@[ (~A)~]"
                     (repl-connection-error-message c) (repl-connection-error-cause c)))))

(define-condition repl-timeout (repl-error)
  ((seconds :initarg :seconds :reader repl-timeout-seconds)
   (request :initarg :request :initform nil :reader repl-timeout-request))
  (:report (lambda (c s)
             (format s "No reply from the image within ~A s~@[ for ~A~]"
                     (repl-timeout-seconds c) (repl-timeout-request c)))))

(define-condition repl-remote-error (repl-error)
  ((reason :initarg :reason :initform nil :reader repl-remote-error-reason))
  (:report (lambda (c s)
             (format s "The image aborted the request~@[: ~A~]" (repl-remote-error-reason c)))))

(define-condition repl-protocol-error (repl-error)
  ((message :initarg :message :reader repl-protocol-error-message))
  (:report (lambda (c s)
             (format s "REPL wire protocol error: ~A" (repl-protocol-error-message c)))))

(define-condition repl-spawn-error (repl-connection-error)
  ((log :initarg :log :initform nil :reader repl-spawn-error-log
        :documentation "Tail of the child image's output."))
  (:report (lambda (c s)
             (format s "Could not start a Lisp image: ~A~@[~%--- image output ---~%~A~]"
                     (repl-connection-error-message c) (repl-spawn-error-log c)))))

(define-condition repl-not-in-debugger (repl-error) ()
  (:report "The image is not in the debugger"))
