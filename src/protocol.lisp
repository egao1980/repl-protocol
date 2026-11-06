(in-package #:repl-protocol)

(defvar *repl-timeout* 60
  "Default seconds to wait for a reply from the image.")

(defclass repl-backend () ()
  (:documentation "A wire dialect (Swank, Slynk, …). Backends subclass."))

(defclass repl-connection ()
  ((backend :initarg :backend :reader connection-backend)
   (process :initarg :process :initform nil :accessor connection-process
            :documentation "PROCESS-PROTOCOL handle when the image was spawned."))
  (:documentation "A live session with one Lisp image."))

;;; Lifecycle

(defgeneric repl-connect (backend &key host port)
  (:documentation "Attach to a running server. → REPL-CONNECTION."))

(defgeneric repl-spawn (backend &key command directory env timeout)
  (:documentation "Start a fresh image that serves BACKEND's wire, attach to it."))

(defgeneric repl-disconnect (connection &key kill)
  (:documentation "Close CONNECTION; with KILL also stop a spawned image."))

(defgeneric repl-connected-p (connection))

(defgeneric repl-connection-info (connection)
  (:documentation "→ CONNECTION-INFO."))

;;; Evaluation

(defgeneric repl-eval (connection string &key package timeout)
  (:documentation "Read and evaluate STRING in the image. → EVAL-RESULT, or a
DEBUGGER-STATE when evaluation entered the debugger."))

(defgeneric repl-interrupt (connection &key thread)
  (:documentation "Interrupt THREAD (default: the one running the last request)."))

(defgeneric repl-compile-string (connection string &key package filename position)
  (:documentation "Compile STRING as if it were at POSITION of FILENAME. → COMPILE-RESULT."))

(defgeneric repl-compile-file (connection pathname &key load)
  (:documentation "→ COMPILE-RESULT."))

(defgeneric repl-load-system (connection name)
  (:documentation "ASDF-load system NAME in the image. → EVAL-RESULT or DEBUGGER-STATE."))

;;; Debugger

(defgeneric repl-debugger-state (connection)
  (:documentation "Innermost active DEBUGGER-STATE, or NIL."))

(defgeneric repl-backtrace (connection &key start end))
(defgeneric repl-frame-locals (connection frame))
(defgeneric repl-frame-source (connection frame))
(defgeneric repl-eval-in-frame (connection frame string &key package))

(defgeneric repl-invoke-restart (connection restart &key level)
  (:documentation "Invoke restart index RESTART of debugger LEVEL (default innermost).
→ the result of the interrupted request: EVAL-RESULT or another DEBUGGER-STATE."))

(defgeneric repl-return-from-frame (connection frame string))
(defgeneric repl-restart-frame (connection frame))

;;; Inspector

(defgeneric repl-inspect-result (connection result &optional index)
  (:documentation "Inspect value INDEX of an earlier EVAL-RESULT. → INSPECTOR-VIEW."))

(defgeneric repl-inspect (connection string &key package)
  (:documentation "Evaluate STRING and inspect the value. → INSPECTOR-VIEW."))

(defgeneric repl-inspect-part (connection index))
(defgeneric repl-inspector-action (connection index))
(defgeneric repl-inspector-pop (connection))
(defgeneric repl-inspector-next (connection))
(defgeneric repl-inspect-frame-var (connection frame var))
(defgeneric repl-inspect-condition (connection))

;;; Introspection

(defgeneric repl-describe (connection name &key package))
(defgeneric repl-documentation (connection name &key package))
(defgeneric repl-arglist (connection name &key package))
(defgeneric repl-apropos (connection pattern &key package external-only))
(defgeneric repl-completions (connection prefix &key package))
(defgeneric repl-macroexpand (connection string &key package all))

(defgeneric repl-xref (connection type name &key package)
  (:documentation "TYPE is one of :CALLS :REFERENCES :BINDS :SETS :MACROEXPANDS
:SPECIALIZES (who-calls NAME, …). → list of XREF-ENTRY."))

(defgeneric repl-find-definitions (connection name &key package)
  (:documentation "→ list of XREF-ENTRY (definition specifier + location)."))

;;; Threads

(defgeneric repl-list-threads (connection))
(defgeneric repl-kill-thread (connection index))

;;; Streams / events

(defgeneric repl-output (connection &key clear)
  (:documentation "Output written by the image outside eval results (e.g. other threads)."))

(defgeneric repl-events (connection &key clear)
  (:documentation "Unhandled wire events, oldest first, as REPL-EVENTs."))

(defgeneric repl-indentation (connection)
  (:documentation "Indentation hints sent by the image: alist of name → spec."))
