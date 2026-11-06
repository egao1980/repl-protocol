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

(defmethod repl-invoke-restart ((conn swank-family-connection) restart &key level)
  (let* ((state (debugger-thread conn))
         (request (debugger-state-request-id state))
         (parser (or (and request (gethash request (connection-parsers conn))) #'identity))
         (serial (bt2:with-lock-held ((wire-lock conn)) (wire-debug-serial conn))))
    (wire-send conn (list :emacs-rex (rpc conn :invoke-restart (or level (debugger-state-level state)) restart)
                          "COMMON-LISP-USER" (debugger-state-thread state)
                          (bt2:with-lock-held ((wire-lock conn)) (incf (wire-next-id conn)))))
    (if request
        (multiple-value-bind (status value) (wire-await conn request :serial serial)
          (unless (eq status :debug) (remhash request (connection-parsers conn)))
          (result-from conn status value parser))
        (make-instance 'eval-result :values '() :aborted nil))))

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
