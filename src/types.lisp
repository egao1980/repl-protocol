(in-package #:repl-protocol)

;;; Plain CLOS values returned by the protocol. Everything printed by the
;;; image arrives as strings; nothing from the image is interned locally.

(defclass source-location ()
  ((file :initarg :file :initform nil :reader source-location-file)
   (position :initarg :position :initform nil :reader source-location-position
             :documentation "0-based character offset in FILE, when known.")
   (line :initarg :line :initform nil :reader source-location-line)
   (snippet :initarg :snippet :initform nil :reader source-location-snippet)
   (error :initarg :error :initform nil :reader source-location-error
          :documentation "Why the location is unknown, as reported by the image.")))

(defmethod print-object ((l source-location) stream)
  (print-unreadable-object (l stream :type t)
    (if (source-location-error l)
        (format stream "error: ~A" (source-location-error l))
        (format stream "~A~@[@~D~]" (source-location-file l) (source-location-position l)))))

(defclass connection-info ()
  ((pid :initarg :pid :initform nil :reader connection-info-pid)
   (implementation :initarg :implementation :initform nil :reader connection-info-implementation)
   (version :initarg :version :initform nil :reader connection-info-version)
   (features :initarg :features :initform '() :reader connection-info-features
             :documentation "Feature names as strings.")
   (package :initarg :package :initform nil :reader connection-info-package)
   (server-version :initarg :server-version :initform nil :reader connection-info-server-version)))

(defmethod print-object ((i connection-info) stream)
  (print-unreadable-object (i stream :type t)
    (format stream "~A ~A pid ~A" (connection-info-implementation i)
            (connection-info-version i) (connection-info-pid i))))

(defclass eval-result ()
  ((values :initarg :values :initform '() :reader eval-result-values
           :documentation "Printed representations of the returned values.")
   (output :initarg :output :initform "" :reader eval-result-output)
   (id :initarg :id :initform nil :reader eval-result-id
       :documentation "Server-side handle of the values, for REPL-INSPECT-RESULT.")
   (aborted :initarg :aborted :initform nil :reader eval-result-aborted-p)
   (reason :initarg :reason :initform nil :reader eval-result-reason)))

(defmethod print-object ((r eval-result) stream)
  (print-unreadable-object (r stream :type t)
    (if (eval-result-aborted-p r)
        (format stream "aborted~@[: ~A~]" (eval-result-reason r))
        (format stream "~{~A~^, ~}" (eval-result-values r)))))

(defclass restart-info ()
  ((index :initarg :index :reader restart-info-index)
   (name :initarg :name :reader restart-info-name)
   (description :initarg :description :reader restart-info-description)))

(defclass frame-info ()
  ((index :initarg :index :reader frame-info-index)
   (description :initarg :description :reader frame-info-description)
   (restartable :initarg :restartable :initform nil :reader frame-info-restartable-p)))

(defclass frame-local ()
  ((name :initarg :name :reader frame-local-name)
   (id :initarg :id :initform 0 :reader frame-local-id)
   (value :initarg :value :reader frame-local-value)))

(defclass debugger-state ()
  ((thread :initarg :thread :reader debugger-state-thread)
   (level :initarg :level :reader debugger-state-level)
   (condition :initarg :condition :reader debugger-state-condition
              :documentation "Printed condition.")
   (condition-type :initarg :condition-type :initform nil :reader debugger-state-condition-type)
   (restarts :initarg :restarts :initform '() :reader debugger-state-restarts)
   (frames :initarg :frames :initform '() :reader debugger-state-frames)
   (request-id :initarg :request-id :initform nil :accessor debugger-state-request-id
               :documentation "Pending request that entered this debugger level, if known."))
  (:documentation "The image entered the debugger. Not a condition: the caller
decides what to do (invoke a restart, inspect frames, …)."))

(defmethod print-object ((d debugger-state) stream)
  (print-unreadable-object (d stream :type t)
    (format stream "level ~D thread ~A: ~A" (debugger-state-level d)
            (debugger-state-thread d) (debugger-state-condition d))))

(defclass inspector-part ()
  ((kind :initarg :kind :reader inspector-part-kind :type (member :text :value :action))
   (text :initarg :text :reader inspector-part-text)
   (index :initarg :index :initform nil :reader inspector-part-index
          :documentation "Part or action index for :VALUE / :ACTION parts.")))

(defclass inspector-view ()
  ((title :initarg :title :reader inspector-view-title)
   (id :initarg :id :initform nil :reader inspector-view-id)
   (parts :initarg :parts :initform '() :reader inspector-view-parts)))

(defmethod print-object ((v inspector-view) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "~A (~D parts)" (inspector-view-title v) (length (inspector-view-parts v)))))

(defclass compiler-note ()
  ((severity :initarg :severity :reader compiler-note-severity
             :documentation "Keyword such as :ERROR :WARNING :STYLE-WARNING :NOTE.")
   (message :initarg :message :reader compiler-note-message)
   (location :initarg :location :initform nil :reader compiler-note-location)))

(defclass compile-result ()
  ((success :initarg :success :reader compile-result-success-p)
   (notes :initarg :notes :initform '() :reader compile-result-notes)
   (duration :initarg :duration :initform nil :reader compile-result-duration)))

(defmethod print-object ((r compile-result) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~:[failed~;ok~] ~D note~:P" (compile-result-success-p r)
            (length (compile-result-notes r)))))

(defclass xref-entry ()
  ((name :initarg :name :reader xref-entry-name)
   (location :initarg :location :initform nil :reader xref-entry-location)))

(defclass thread-info ()
  ((index :initarg :index :reader thread-info-index)
   (id :initarg :id :initform nil :reader thread-info-id)
   (name :initarg :name :reader thread-info-name)
   (status :initarg :status :initform nil :reader thread-info-status)))

(defclass repl-event ()
  ((kind :initarg :kind :reader repl-event-kind)
   (data :initarg :data :initform nil :reader repl-event-data)))
