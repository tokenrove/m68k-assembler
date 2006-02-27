
(in-package :m68k-assembler)

;;;; LEXER BOOKKEEPING

(defstruct lexer-state
  (stream)
  (filename)
  (line 0)
  (column 0)
  (current-string nil))

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
  (setf (lexer-state-column (first *lexer-states*)) 0)
  (incf (lexer-state-line (first *lexer-states*))))

(defun lexer-next-column ()
  (incf (lexer-state-column (first *lexer-states*))))

(defun update-lexer-column (c)
  (assert (<= (lexer-state-column (first *lexer-states*)) c))
  (setf (lexer-state-column (first *lexer-states*)) c))

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

;;;; MAIN LEXER FUNCTION

(defun next-token ()
  (handler-bind ((end-of-file (lambda (condition)
				(pop *lexer-states*)
				(if *lexer-states*
				    (next-token)
				    (error condition)))))
    (unless *lexer-states*
      (error 'end-of-file :stream nil))
    (multiple-value-bind (string column)
	(ensure-lexer-data (first *lexer-states*))
      (when (and (zerop column) (not *lexer-seen-only-whitespace-this-line-p*))
	(return-from next-token (maybe-return-$)))
      (atypecase (devpac-lexer string #'update-lexer-column :start column)
	(atom (next-token))
	(t (setf *lexer-seen-only-whitespace-this-line-p* nil)
	   it)))))

(defun ensure-lexer-data (state)
  (symbol-macrolet ((string (lexer-state-current-string state)))
      (unless (and string
		   (< (lexer-state-column state)
		      (length string)))
	(setf string
	      (read-possibly-escaped-line (lexer-state-stream state))))
    (values string (lexer-state-column state))))

(defun read-possibly-escaped-line (stream)
  (loop for line = (read-line stream) then
	(concatenate 'string
		     (make-array (list (1- (length line)))
				 :displaced-to line)
		     (read-line stream))
	do (lexer-next-line)
	unless (and (plusp (length line))
		    (char= (schar line (1- (length line))) #\\))
	return line))

(cl-ppcre-lex:deflexer devpac-lexer
  ("^[ \\t]*\\*.*$" () 'whitespace)
  ("[ \\t]+" () 'whitespace)
  ("([\\r\\n\\f]+|$)" () 'whitespace)
  ("[;]+.*$" () 'whitespace)
  ("([\\-():,#+/*|&^~])"
   (single) (assert single)
   (let ((it (find (char single 0)
		   '((#\( open) (#\) close) (#\: colon) (#\, comma)
		     (#\# hash) (#\+ +) (#\- -) (#\/ /) (#\* *) (#\| or)
		     (#\& &) (#\^ ^) (#\~ ~))
		   :key #'car)))
     (assert it)
     (make-token (second it) (first it))))
  ("([0-9]+)" (digits) (assert digits)
   (make-token 'constant (parse-integer digits)))
  ("\\$([0-9A-Fa-f]+)" (digits) (assert digits)
   (make-token 'constant (parse-integer digits :radix 16)))
  ("%([01]+)" (digits) (assert digits)
   (make-token 'constant (parse-integer digits :radix 2)))
  ("([A-Za-z0-9_@.]+)(\\.[bBwWlL])"
   (string modifier) (assert (and string modifier))
   (setf modifier (when modifier (string-to-modifier modifier)))
   (acond ((register-p (register-substitutions string))
	   (make-token 'register (list (register-substitutions string)
				       modifier)))
	  ((opcode-p string) (make-token 'opcode (list string modifier)))
	  ((pseudo-op-p string) (make-token 'pseudo-op (list string modifier)))
	  (t (make-token 'symbol string))))
  ("([A-Za-z0-9_@.]+)"
   (string) (assert string)
   (acond ((register-p (register-substitutions string))
	   (make-token 'register (list (register-substitutions string) nil)))
	  ((opcode-p string) (make-token 'opcode (list string nil)))
	  ((pseudo-op-p string) (make-token 'pseudo-op (list string nil)))
	  (t (make-token 'symbol string))))
  ("=" () (make-token 'pseudo-op (list "=" nil)))
  ("\"([^\"]*)\"" (string) (make-token 'string string))
  ("<<" () (make-token '<< nil))
  (">>" () (make-token '>> nil))
  ("<([^<>]*)>" (string) (make-token 'symbol string))
  ("(\\\\[1-9A-Za-z@])" (string) (make-token 'symbol string)))

;;;; LEXER HELPERS

(defun make-token (symbol value)
  (list symbol value (copy-lexer-state (first *lexer-states*))))

(defun terminal-p (symbol)
  (member symbol '(open close colon comma hash + - / * or & ^ ~ << >>
		   constant symbol register opcode pseudo-op $)))

(defun is-position-info-p (x) (lexer-state-p x))

(defun string-to-modifier (string)
  (let ((start (if (char= (char string 0) #\.) 1 0)))
    (cond ((string-equal string "b" :start1 start) 'byte)
	  ((string-equal string "w" :start1 start) 'word)
	  ((string-equal string "l" :start1 start) 'long))))

(defun maybe-return-$ ()
  (cond (*lexer-seen-only-whitespace-this-line-p* 'whitespace)
	(t (setf *lexer-seen-only-whitespace-this-line-p* t)
	   (make-token '$ nil))))
