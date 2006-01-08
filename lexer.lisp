
(in-package :m68k-assembler)

;;;; TOKEN PARAMETERS

(defparameter *lexer-terminals*
  '(open close colon comma hash + - / * or & ^ ~ << >>
    constant symbol register opcode pseudo-op $))

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
    #\_ #\. #\= #\\ #\@)
  "Characters permitted in a symbol, register, or opcode.")
(defparameter *lexer-int-characters* `(,@(list-char-range #\0 #\9))
  "Characters permitted in an integer.")
(defparameter *lexer-whitespace-characters* '(#\Space #\Tab))


;;;; LEXER BOOKKEEPING

(defstruct lexer-state
  (stream)
  (filename)
  (line 1)
  (column 1))

(defvar *lexer-states* nil)
;;; *l-s-o-w-t-l-p* doesn't need to keep multi-file state.
(defvar *lexer-seen-only-whitespace-this-line-p*)

(defmacro with-lexer ((filename) &body body)
  `(unwind-protect (progn
		     (init-lexer ,filename)
		     ,@body)
    (close-lexer)))

(defun init-lexer (filename)
  (setf *lexer-states* nil)
  (nested-lexing filename))

(defun close-lexer ()
  (do ((s #1=(pop *lexer-states*) #1#))
      ((null s))
    (close (lexer-state-stream s))))

(defun lexer-next-line ()
  (setf *lexer-seen-only-whitespace-this-line-p* t
	(lexer-state-column (first *lexer-states*)) 1)
  (incf (lexer-state-line (first *lexer-states*))))

(defun lexer-next-column ()
  (incf (lexer-state-column (first *lexer-states*))))

(defun nested-lexing (filename)
  (cond ((find filename *lexer-states* :test #'string-equal
	       :key #'lexer-state-filename)
	 (warn "~A is already in the chain of INCLUDES!  Ignoring it..."
	       filename))
	(t
	 (setf *lexer-seen-only-whitespace-this-line-p* t)
	 (push (make-lexer-state :stream (open filename)
				 :filename filename)
	       *lexer-states*))))

;;;; INTERMEDIARY LEXING FUNCTIONS (EATERS)

(defun eat-whitespace (stream)
  (do ((next-char #1=(peek-char nil stream) #1#))
      ((not (find next-char *lexer-whitespace-characters*)))
    (read-char stream)
    (lexer-next-column)))

(defun eat-string (stream &optional (start #\") (end #\"))
  "Reads a backslash-escaped string from STREAM, delimited by the
characters START and END (which default to quotes)."
  (assert (eql (read-char stream) start))
  (lexer-next-column)
  (do ((next-char #1=(progn (lexer-next-column)
			    (read-char stream)) #1#)
       (string (make-array '(0) :element-type 'character
			   :adjustable t :fill-pointer 0)))
      ((eql next-char end) string)
    (acase next-char
      (#\\ (vector-push-extend (read-char stream) string)
	   (lexer-next-column))
      (t (vector-push-extend it string)))))

(defconstant +int-conversion-table+ (load-time-value
				     "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
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

(defun next-token ()
  (handler-case
      (if *lexer-states*
	  (next-token-1 (lexer-state-stream (first *lexer-states*)))
	  (signal 'end-of-file))
    (end-of-file nil
      (pop *lexer-states*)
      (if *lexer-states* (next-token) (signal 'end-of-file)))))

(defun next-token-1 (stream)
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
	      (next-token-1 stream))
	     (t (read-char stream)
		(make-token (cadr it) lookahead))))
      ;; The order of the following few cases is significant.
      ((member lookahead *lexer-int-characters*)
       (make-token 'constant (eat-integer stream)))
      ((eql lookahead #\$) (make-token 'constant (eat-hex stream)))
      ((eql lookahead #\%) (make-token 'constant (eat-binary stream)))
      ((member lookahead *lexer-word-characters*)
       (let ((token (eat-symbol stream)))
	 (multiple-value-bind (string modifier) (munge-modifier token)
	   (setf modifier (string-to-modifier modifier))
	   (cond ((register-p (register-substitutions string))
		  (make-token 'register (list (register-substitutions string)
					      modifier)))
		 ((opcode-p string) (make-token 'opcode
						(list string modifier)))
		 ((pseudo-op-p string) (make-token 'pseudo-op
						   (list string modifier)))
		 (t (make-token 'symbol token))))))
      ((eql lookahead #\")		; string
       (make-token 'constant (eat-string stream)))

      ;; Little special cases.
      ((eql lookahead #\<)		; expect < or macro parameter
       (read-char stream)
       (cond ((eql (peek-char nil stream) #\<)
	      (read-char stream)
	      (make-token '<< nil))
	     ;; XXX: one problem here is that we don't deal with
	     ;; escaped <'s inside the string, and I'm not too anxious
	     ;; to do so yet, either.
	     (t (unread-char #\< stream)
		(make-token 'symbol (eat-string stream #\< #\>)))))
      ((eql lookahead #\>)		; expect >
       (read-char stream)
       (assert (eql (read-char stream) #\>))
       (make-token '>> nil))
      ((eql lookahead #\\)
       ;; if it's a macro parameter (\[1-9A-Za-z] or \@) store it as a
       ;; symbol ... MACRO will know what to do with it.
       ;; also there's \symbol and \$symbol but I'm not sure if we'll
       ;; support them yet.
       ;; otherwise, it might be a line continuation token. XXX
       (read-char stream)
       (when (or (char-equal (peek-char nil stream) #\@)
		 (member (peek-char nil stream) *lexer-word-characters*))
	 (make-token 'symbol (concatenate 'string "\\" (eat-symbol stream)))))
      ((eql lookahead #\;)		; comment
       (read-line stream)
       (maybe-return-$ stream))
      ((eql lookahead #\Return)		; stupid ^M
       (read-line stream)
       (maybe-return-$ stream))
      ((eql lookahead #\Newline)
       (read-line stream)
       (maybe-return-$ stream)))))

;;;; LEXER HELPERS

(defun maybe-return-$ (stream)
  (cond (*lexer-seen-only-whitespace-this-line-p*
	 (lexer-next-line) (next-token-1 stream))
	(t (lexer-next-line) (make-token '$ nil))))

(defun make-token (symbol value)
  (list symbol value (copy-lexer-state (first *lexer-states*))))

(defun terminal-p (symbol) (member symbol *lexer-terminals*))

(defun is-position-info-p (x) (lexer-state-p x))

(defun string-to-modifier (string)
  (cond ((string-equal string "b") 'byte)
	((string-equal string "w") 'word)
	((string-equal string "l") 'long)))

