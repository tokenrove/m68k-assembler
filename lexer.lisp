
(in-package :m68k-assembler)

;;;; TOKEN PARAMETERS

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


;;;; LEXER BOOKKEEPING

;;; XXX: keep track of line/column numbers.  I think position will
;;; become a stack rather than a single list as soon as we handle
;;; INCLUDE directives and so on.  Except *l-s-o-w-t-l-p* doesn't need
;;; to keep multi-file state.
(defvar *lexer-position*)
(defvar *lexer-seen-only-whitespace-this-line-p*)

(defun init-lexer (file)
  (setf *lexer-position* (list file 1 1)
	*lexer-seen-only-whitespace-this-line-p* t))

(defun lexer-next-line ()
  (setf *lexer-seen-only-whitespace-this-line-p* t)
  (incf (second *lexer-position*)))
(defun lexer-next-column () (incf (third *lexer-position*)))



;;;; INTERMEDIARY LEXING FUNCTIONS (EATERS)

(defun eat-whitespace (stream)
  (do ((next-char #1=(peek-char nil stream) #1#))
      ((not (find next-char *lexer-whitespace-characters*)))
    (read-char stream)
    (lexer-next-column)))

(defun eat-string (stream)
  "Reads a quote-delimited, backslash-escaped string from STREAM."
  (assert (eql (read-char stream) #\"))
  (lexer-next-column)
  (do ((next-char #1=(progn (lexer-next-column)
			    (read-char stream)) #1#)
       (string (make-array '(0) :element-type 'character
			   :adjustable t :fill-pointer 0)))
      ((eql next-char #\") string)
    (acase next-char
      (#\\ (vector-push-extend (read-char stream) string)
	   (lexer-next-column))
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
		 (digit-to-int (read-char stream) radix)))
    (lexer-next-column)))

(defun eat-hex (stream)
  (assert (eql (read-char stream) #\$))
  (lexer-next-column)
  (eat-integer stream 16))

(defun eat-binary (stream)
  (assert (eql (read-char stream) #\%))
  (lexer-next-column)
  (eat-integer stream 2))

(defun eat-symbol (stream)
  (do ((next-char #1=(peek-char nil stream) #1#)
       (symbol (make-array '(0) :element-type 'character
			   :adjustable t :fill-pointer 0)))
      ((not (find next-char *lexer-word-characters*)) symbol)
    (lexer-next-column)
    (vector-push-extend (read-char stream) symbol)))


;;;; MAIN LEXER FUNCTION

(defun next-token (stream)
  (eat-whitespace stream)
  (let ((lookahead (peek-char nil stream)))
    ;; XXX ugly first-column asterix hack
    (unless (member lookahead '(#\* #\Return #\Newline #\;))
      (setf *lexer-seen-only-whitespace-this-line-p* nil))
    (acond
      ((find lookahead *lexer-single-char-tokens* :key #'car)
       ;; XXX ugly first-column asterix hack
       (cond (*lexer-seen-only-whitespace-this-line-p*
	      (read-line stream)
	      (lexer-next-line)
	      (next-token stream))
	     (t (read-char stream)
		(make-token (cadr it) lookahead))))
      ;; The order of the following few cases is significant.
      ((member lookahead *lexer-int-characters*)
       (make-token 'constant (eat-integer stream)))
      ((eql lookahead #\$) (make-token 'constant (eat-hex stream)))
      ((eql lookahead #\%) (make-token 'constant (eat-binary stream)))
      ((member lookahead *lexer-word-characters*)
       (let ((token (eat-symbol stream)))
	 (acond ((register-p token) (make-token 'register it))
		((opcode-p token) (make-token 'opcode it))
		((pseudo-op-p token) (make-token 'pseudo-op it))
		(t (make-token 'symbol token)))))
      ((eql lookahead #\")		; string
       (make-token 'constant (eat-string stream)))

      ;; Little special cases.
      ((eql lookahead #\<)		; expect <
       (read-char stream)
       (assert (eql (read-char stream) #\<))
       (make-token '<< nil))
      ((eql lookahead #\>)		; expect >
       (read-char stream)
       (assert (eql (read-char stream) #\>))
       (make-token '>> nil))
      ((eql lookahead #\;)		; comment
       (read-line stream)
       (maybe-return-$ stream))
      ((eql lookahead #\Return)		; stupid ^M
       (read-line stream)
       (maybe-return-$ stream))
      ((eql lookahead #\Newline)
       (read-line stream)
       (maybe-return-$ stream)))))

(defun maybe-return-$ (stream)
  (cond (*lexer-seen-only-whitespace-this-line-p*
	 (lexer-next-line) (next-token stream))
	(t (lexer-next-line) (make-token '$ nil))))

(defun make-token (symbol value)
  (list symbol value (copy-list *lexer-position*)))
