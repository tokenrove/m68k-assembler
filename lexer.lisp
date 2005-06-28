
(in-package :m68k-assembler)

'(symbol opcode pseudo-op register
  constant
  colon comma open close + - hash / * ^ or & ~
  << >>
  $)

(defparameter *lexer-single-char-tokens*
  '((#\( open)
    (#\) close)
    (#\: colon)
    (#\, comma)
    (#\# hash)
    (#\+ +) (#\- -) (#\/ /) (#\* *)
    (#\| or) (#\& &) (#\^ ^) (#\~ ~)))

(defun list-char-range (start end)
  (do ((x (char-code start) (1+ x))
       (l nil))
      ((> x (char-code end)) l)
    (push (code-char x) l)))

(defparameter *lexer-word-characters*
  `(,@(list-char-range #\A #\Z)
    ,@(list-char-range #\a #\z)
    ,@(list-char-range #\0 #\9)
    #\_ #\.)
  "Characters permitted in a symbol, register, or opcode.")
(defparameter *lexer-int-characters* `(,@(list-char-range #\0 #\9))
  "Characters permitted in an integer.")
(defparameter *lexer-whitespace-characters* '(#\Space #\Tab))


(defun eat-whitespace (stream)
  (do ((next-char #1=(peek-char nil stream) #1#))
      ((not (find next-char *lexer-whitespace-characters*)))
    (read-char stream)))

(defun eat-string (stream)
  "Reads a quote-delimited, backslash-escaped string from STREAM."
  (assert (eql (read-char stream) #\"))
  (do ((next-char #1=(peek-char nil stream) #1#)
       (string (make-array '(0) :element-type 'character
			   :adjustable t :fill-pointer 0)))
      ((eql next-char #\") (coerce string 'string))
    (acase (read-char stream)
      (#\\ (vector-push-extend (read-char stream) string))
      (t (vector-push-extend it string)))))

(defconstant +int-conversion-table+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defun digit-to-int (char &optional (radix 10))
  (position char +int-conversion-table+ :test #'char-equal :end radix))

(defun eat-integer (stream &optional (radix 10))
  "If the next character is a digit, read digits until the next
character is not a digit."
  (do ((next-char #1=(peek-char nil stream) #1#) int)
      ((not (digit-to-int next-char radix)) int)
    (setf int (+ (* (or int 0) radix)
		 (digit-to-int (read-char stream) radix)))))

(defun eat-hex (stream)
  (assert (eql (read-char stream) #\$))
  (eat-integer stream 16))

(defun eat-symbol (stream)
  (do ((next-char #1=(peek-char nil stream) #1#)
       (symbol (make-array '(0) :element-type 'character
			   :adjustable t :fill-pointer 0)))
      ((not (find next-char *lexer-word-characters*)) symbol)
    (vector-push-extend (read-char stream) symbol)))


;;; We might want to put these somewhere more machine-related.
(defun register-p (string)
  (let ((register-list '("a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7"
			 "d0" "d1" "d2" "d3" "d4" "d5" "d6" "d7"
			 "pc" "sp")))
    (find string register-list :test #'string-equal)))

(defun opcode-p (string)
  (find string *asm-opcode-table* :key #'car :test #'string-equal))

(defun pseudo-op-p (string)
  (find string *asm-pseudo-op-table* :key #'car :test #'string-equal))


;;; XXX: keep track of line/column numbers

(defun next-token (stream)
  (eat-whitespace stream)
  (let ((lookahead (peek-char nil stream)))
    (acond
      ((find lookahead *lexer-single-char-tokens* :key #'car)
       ;; XXX first-column asterix hack
       (read-char stream)
       (cons (cadr it) lookahead))
      ;; The order of the following few cases is significant.
      ((member lookahead *lexer-int-characters*)
       (cons 'constant (eat-integer stream)))
      ((eql lookahead #\$)		; hex
       (cons 'constant (eat-hex stream)))
      ((member lookahead *lexer-word-characters*)
       (let ((token (eat-symbol stream)))
	 (cond ((register-p token) (cons 'register token))
	       ((opcode-p token) (cons 'opcode token))
	       ((pseudo-op-p token) (cons 'pseudo-op token))
	       (t (cons 'symbol token)))))
      ((eql lookahead #\")		; string
       (cons 'constant (eat-string stream)))

      ;; Little special cases.
      ((eql lookahead #\<)		; expect <
       (read-char stream)
       (assert (eql (read-char stream) #\<))
       (cons '<< nil))
      ((eql lookahead #\>)		; expect >
       (read-char stream)
       (assert (eql (read-char stream) #\>))
       (cons '>> nil))
      ((eql lookahead #\;)		; comment
       (read-line stream)
       (cons '$ nil))
      ((eql lookahead #\Newline)
       ;; XXX Does this handle DOS-style lines as well?
       (read-char stream)
       (cons '$ nil)))))

;; read a character
;;   if it's whitespace, ignore it
;;   if it's a comment character, finish up
;;   if it's a special character, return that token
;;   special hack for $DEADBEEF
;;   if it's an integer, read until non-integer character,
;;      return constant
;;   if it's a quote, read until end-quote, w/backslash escaping,
;;      return constant
;;   if it's another character, read until non-word character,
;;      look up register or opcode or pseudo-op
;;      otherwise, symbol.


;; start reading a line...
;;   until we find whitespace,
;;     read a label.
;;   strip : from label if it was there.
;;   push label.
;;   skip whitespace.
;;   read op until whitespace.
;;   determine whether op is pseudo or opcode.
;;   push op.
;;   read and push operands until end of line.
;;   push $.
