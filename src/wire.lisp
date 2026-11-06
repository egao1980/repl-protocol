(in-package #:repl-protocol)

;;; The Swank/Slynk wire: a 6-digit hex length (UTF-8 octets) followed by a
;;; printed s-expression. Incoming forms are read by a small dedicated
;;; reader: no #., no interning. Symbols become WIRE-SYMBOLs, except NIL,
;;; T and keywords that already exist locally.

(defstruct (wire-symbol (:constructor make-wire-symbol (package name)))
  "A symbol on the wire. PACKAGE is a name string, \"KEYWORD\", or NIL (#:sym)."
  (package nil :read-only t)
  (name "" :type string :read-only t))

(defmethod print-object ((s wire-symbol) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "#<WIRE ~@[~A~]~:[#~;::~]~A>"
              (wire-symbol-package s) (wire-symbol-package s) (wire-symbol-name s))))

(defun wsym (package name)
  "Wire symbol PACKAGE::NAME (both upcased)."
  (make-wire-symbol (and package (string-upcase package)) (string-upcase name)))

(defun symbol-name-of (x)
  (cond ((wire-symbol-p x) (wire-symbol-name x))
        ((symbolp x) (symbol-name x))))

(defun wire-symbol= (x name)
  "True when wire value X is a symbol (wire or local) named NAME."
  (let ((n (symbol-name-of x)))
    (and n (string-equal n name))))

(defun wire-plist-get (plist key)
  "Value after the first key in PLIST whose symbol name is KEY."
  (loop for (k v) on plist by #'cddr
        when (wire-symbol= k key) do (return v)))

;;; Reader

(defun delimiterp (c)
  (or (member c '(#\( #\) #\" #\' #\; #\` #\,))
      (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed))))

(defun wire-error (control &rest args)
  (error 'repl-protocol-error :message (apply #'format nil control args)))

(defun number-token-p (token)
  "True when TOKEN has decimal integer, ratio or float syntax (CLHS 2.3.1),
so READ-FROM-STRING on it can only return a number."
  (let ((i 0) (n (length token)))
    (labels ((digits ()
               (let ((s i))
                 (loop while (and (< i n) (digit-char-p (char token i))) do (incf i))
                 (- i s)))
             (opt (chars)
               (when (and (< i n) (find (char token i) chars)) (incf i) t)))
      (opt "+-")
      (let ((int (digits)))
        (cond ((= i n) (plusp int))
              ((char= (char token i) #\/)
               (incf i) (and (plusp int) (plusp (digits)) (= i n)))
              (t
               (let ((frac (if (opt ".") (digits) 0)))
                 (cond ((= i n) (or (plusp int) (plusp frac)))
                       ((and (or (plusp int) (plusp frac)) (opt "eEdDfFsSlL"))
                        (opt "+-")
                        (and (plusp (digits)) (= i n)))
                       (t nil)))))))))

(defun parse-number-token (token)
  (handler-case
      (let ((*read-eval* nil)
            (*read-base* 10)
            (*read-default-float-format* 'double-float))
        (let ((v (let ((*package* (find-package "KEYWORD"))) (read-from-string token))))
          (if (numberp v) v (wire-error "bad number ~S" token))))
    (reader-error () (wire-error "bad number ~S" token))))

(defun intern-free-symbol (package name)
  "NIL / T / existing keyword, or a WIRE-SYMBOL."
  (cond ((and (member package '(nil "COMMON-LISP" "CL") :test #'equal)
              (string= name "NIL"))
         nil)
        ((and (member package '(nil "COMMON-LISP" "CL") :test #'equal)
              (string= name "T"))
         t)
        ((equal package "KEYWORD")
         (multiple-value-bind (sym status) (find-symbol name "KEYWORD")
           (if status sym (make-wire-symbol "KEYWORD" name))))
        (t (make-wire-symbol package name))))

(defun read-wire-form (string &key (start 0))
  "Read one wire form from STRING. → (values form next-position)."
  (let ((pos start)
        (len (length string)))
    (labels ((peek () (and (< pos len) (char string pos)))
             (next () (prog1 (char string pos) (incf pos)))
             (skip ()
               (loop while (< pos len)
                     do (let ((c (char string pos)))
                          (cond ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf pos))
                                ((char= c #\;) (loop while (and (< pos len) (char/= (char string pos) #\Newline))
                                                     do (incf pos)))
                                (t (return))))))
             (read-token ()
               ;; → (values token-with-case-folding package-marker-positions)
               (let ((out (make-string-output-stream))
                     (markers '())
                     (i 0))
                 (loop while (and (< pos len) (not (delimiterp (peek))))
                       do (let ((c (next)))
                            (case c
                              (#\\ (when (< pos len) (write-char (next) out)) (incf i))
                              (#\| (loop while (and (< pos len) (char/= (peek) #\|))
                                         do (let ((d (next)))
                                              (when (char= d #\\) (setf d (next)))
                                              (write-char d out) (incf i)))
                                   (when (< pos len) (next)))
                              (#\: (push i markers) (write-char c out) (incf i))
                              (t (write-char (char-upcase c) out) (incf i)))))
                 (values (get-output-stream-string out) (nreverse markers))))
             (form ()
               (skip)
               (unless (< pos len) (wire-error "unexpected end of message"))
               (let ((c (peek)))
                 (case c
                   (#\( (next) (read-list))
                   (#\) (wire-error "unexpected ) at ~D" pos))
                   (#\" (next) (read-string))
                   (#\' (next) (list (make-wire-symbol "COMMON-LISP" "QUOTE") (form)))
                   (#\# (next) (read-dispatch))
                   (t (read-atom)))))
             (read-list ()
               (let ((items '()) (tail nil))
                 (loop
                   (skip)
                   (unless (< pos len) (wire-error "unterminated list"))
                   (cond ((char= (peek) #\)) (next) (return))
                         ((and (char= (peek) #\.) (< (1+ pos) len) (delimiterp (char string (1+ pos))))
                          (next) (setf tail (form)) (skip)
                          (unless (eql (peek) #\)) (wire-error "bad dotted list"))
                          (next) (return))
                         (t (push (form) items))))
                 (let ((l (nreverse items)))
                   (when tail (setf (cdr (last l)) tail))
                   l)))
             (read-string ()
               (with-output-to-string (out)
                 (loop
                   (unless (< pos len) (wire-error "unterminated string"))
                   (let ((c (next)))
                     (cond ((char= c #\") (return))
                           ((char= c #\\) (write-char (next) out))
                           (t (write-char c out)))))))
             (read-dispatch ()
               (let ((c (and (< pos len) (next))))
                 (case c
                   (#\: (multiple-value-bind (name) (read-token) (make-wire-symbol nil name)))
                   (#\\ (let ((start pos))
                          (next)
                          (loop while (and (< pos len) (not (delimiterp (peek)))) do (next))
                          (let ((name (subseq string start pos)))
                            (or (if (= (length name) 1) (char name 0) (name-char name))
                                (wire-error "unknown character ~S" name)))))
                   (#\( (coerce (read-list) 'vector))
                   (t (wire-error "unsupported #~@[~C~] syntax at ~D" c pos)))))
             (read-atom ()
               (multiple-value-bind (token markers) (read-token)
                 (cond ((null markers)
                        (if (number-token-p token)
                            (parse-number-token token)
                            (intern-free-symbol nil token)))
                       ((zerop (first markers))
                        (intern-free-symbol "KEYWORD" (subseq token (1+ (car (last markers))))))
                       (t (intern-free-symbol (subseq token 0 (first markers))
                                              (subseq token (1+ (car (last markers))))))))))
      (values (form) pos))))

;;; Writer

(defun symbol-token-needs-escape-p (name)
  (or (zerop (length name))
      (some (lambda (c) (or (lower-case-p c) (delimiterp c) (find c "|\\:#"))) name)
      (number-token-p name)))

(defun write-symbol-name (name stream)
  (if (symbol-token-needs-escape-p name)
      (progn (write-char #\| stream)
             (loop for c across name
                   do (when (find c "|\\") (write-char #\\ stream))
                      (write-char c stream))
             (write-char #\| stream))
      (write-string name stream)))

(defun write-wire-form (form stream)
  "Print FORM for the image. Only lists, strings, numbers, characters, NIL,
T, keywords and WIRE-SYMBOLs are allowed."
  (cond ((null form) (write-string "nil" stream))
        ((eq form t) (write-string "t" stream))
        ((stringp form)
         (write-char #\" stream)
         (loop for c across form
               do (when (find c "\"\\") (write-char #\\ stream))
                  (write-char c stream))
         (write-char #\" stream))
        ((integerp form) (format stream "~D" form))
        ((or (rationalp form) (floatp form))
         (let ((*read-default-float-format* 'single-float)) (prin1 form stream)))
        ((characterp form) (let ((*print-readably* t)) (prin1 form stream)))
        ((keywordp form) (write-char #\: stream) (write-symbol-name (symbol-name form) stream))
        ((wire-symbol-p form)
         (cond ((null (wire-symbol-package form)) (write-string "#:" stream))
               ((equal (wire-symbol-package form) "KEYWORD") (write-char #\: stream))
               (t (write-symbol-name (wire-symbol-package form) stream)
                  (write-string "::" stream)))
         (write-symbol-name (wire-symbol-name form) stream))
        ((consp form)
         (write-char #\( stream)
         (loop for (x . rest) on form
               do (write-wire-form x stream)
                  (cond ((null rest))
                        ((consp rest) (write-char #\Space stream))
                        (t (write-string " . " stream) (write-wire-form rest stream))))
         (write-char #\) stream))
        ((vectorp form)
         (write-string "#" stream) (write-wire-form (coerce form 'list) stream))
        (t (error 'repl-protocol-error :message (format nil "cannot send ~S" form)))))

(defun wire-form-string (form)
  (with-output-to-string (s) (write-wire-form form s)))

;;; Framing

(defun encode-message (form)
  "Octets of FORM framed for the wire."
  (let* ((payload (babel:string-to-octets (wire-form-string form) :encoding :utf-8))
         (header (babel:string-to-octets (format nil "~6,'0X" (length payload)) :encoding :latin-1)))
    (concatenate '(vector (unsigned-byte 8)) header payload)))

(defun write-message (stream form)
  (write-sequence (encode-message form) stream)
  (finish-output stream))

(defun read-exactly (stream n)
  (let* ((buf (make-array n :element-type '(unsigned-byte 8)))
         (got (read-sequence buf stream)))
    (when (< got n)
      (error 'repl-connection-error :message "connection closed by the image"))
    buf))

(defun read-message (stream)
  "Read one framed message from binary STREAM. → form."
  (let* ((header (babel:octets-to-string (read-exactly stream 6) :encoding :latin-1))
         (length (handler-case (parse-integer header :radix 16)
                   (parse-error () (wire-error "bad header ~S" header)))))
    (values (read-wire-form (babel:octets-to-string (read-exactly stream length) :encoding :utf-8)))))
