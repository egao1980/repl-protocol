(in-package #:repl-protocol/tests)

(deftest-parametrize read-wire-forms
    ((text expected)
     ("(:return (:ok \"x\") 3)" '(:return (:ok "x") 3))
     ("nil" nil)
     ("t" t)
     ("(1 -2 3/4 1.5 2.5d0)" '(1 -2 3/4 1.5d0 2.5d0))
     ("\"a\\\"b\\\\c\"" "a\"b\\c")
     ("(a . b)" (cons (wsym nil "A") (wsym nil "B")))
     ("#\\a" #\a)
     ("#(1 2)" #(1 2)))
  (ok (equalp (read-wire-form text) expected)))

(deftest wire-symbols-are-not-interned
  (let* ((name "REPL-PROTOCOL-TEST-NEVER-INTERNED")
         (form (read-wire-form (format nil "(swank::~A :~:*~A ~:*~A #:~:*~A 1+ |lower|)" name))))
    (ok (null (find-symbol name :keyword)))
    (ok (null (find-symbol name :cl-user)))
    (ok (every #'wire-symbol-p (butlast form 2)))
    (ok (equal (wire-symbol-package (first form)) "SWANK"))
    (ok (equal (wire-symbol-package (second form)) "KEYWORD"))
    (ok (null (wire-symbol-package (fourth form))))
    (ok (wire-symbol= (fifth form) "1+") "1+ is a symbol, not a number")
    (ok (equal (wire-symbol-name (sixth form)) "lower"))))

(deftest-parametrize unsafe-or-bad-syntax
    ((text)
     ("#.(error \"boom\")")
     ("#<OBJECT>")
     ("(unterminated")
     ("\"open"))
  (ok (signals (read-wire-form text) 'repl-protocol-error)))

(deftest write-wire-forms
  (ok (string= (wire-form-string (list :emacs-rex (list (wsym "swank" "connection-info")) "CL-USER" t 1))
               "(:EMACS-REX (SWANK::CONNECTION-INFO) \"CL-USER\" t 1)"))
  (ok (string= (wire-form-string (list (wsym "p" "odd name") "q\"x")) "(P::|ODD NAME| \"q\\\"x\")"))
  (ok (string= (wire-form-string '(1 . 2)) "(1 . 2)"))
  (ok (signals (wire-form-string (list 'cl-user::not-a-keyword)) 'repl-protocol-error)))

(deftest round-trip
  (let ((form (list :a "λ unicode" 42 (wsym "SWANK" "X") (list (list "nested") :b) nil)))
    (ok (equalp (read-wire-form (wire-form-string form)) form))))

(deftest framing-counts-utf8-octets
  (let* ((octets (encode-message "λ"))
         (header (map 'string #'code-char (subseq octets 0 6))))
    ;; "\"λ\"" is 4 octets: quote, two for lambda, quote
    (ok (string= header "000004"))
    (ok (equal (flexi-streams:with-input-from-sequence (in octets) (read-message in)) "λ"))))
