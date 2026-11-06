(defsystem "repl-protocol"
  :version "0.1.0"
  :description "CLOS protocol for a live Lisp image session: eval, debugger, inspector, xref, threads"
  :author "egao1980"
  :license "MIT"
  :depends-on ("alexandria" "bordeaux-threads" "usocket" "babel" "process-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "protocol")
               (:file "wire")
               (:file "connection")
               (:file "swank-family")
               (:file "spawn"))
  :in-order-to ((test-op (test-op "repl-protocol/tests"))))

(defsystem "repl-protocol/conformance"
  :description "Shared conformance suite for repl-protocol backends (needs a live image)"
  :depends-on ("repl-protocol" "rove")
  :pathname "tests"
  :components ((:file "conformance")))

(defsystem "repl-protocol/tests"
  :depends-on ("repl-protocol" "rove" "flexi-streams")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "wire-test")
               (:file "connection-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
