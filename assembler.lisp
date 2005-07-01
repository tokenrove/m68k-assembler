
(in-package :m68k-assembler)

;;;; PSEUDO-OPS

(defparameter *asm-pseudo-op-table*
  #(("SECTION" code)
    ("INCLUDE" code)
    ("MACRO")
    ("ENDM")
    ("REPT")
    ("ENDR")
    ("DC")
    ("DS")))

;;;; SYMBOL TABLE

(defvar *symbol-table* nil)

(defun add-to-symbol-table (sym value position)
  (cond ((consp sym)
	 (assert (eql (first sym) 'symbol))
	 (setf (gethash (second sym) *symbol-table*)
	       (list value position)))
	(t (error "Not sure how to handle this symbol: ~A." sym))))

(defun get-symbol-value (sym)
  (awhen (gethash sym *symbol-table*) (first it)))

;;;; HELPER FUNCTIONS

(defun resolve-expression (expression)
  "Try and get a numerical value from this expression.  Returns the
further simplified expression, which will be an atom if everything was
resolved."
  (flet ((bin-op (sym fn)
	   (assert (= (length expression) 3))
	   (let ((a (resolve-expression (second expression)))
		 (b (resolve-expression (third expression))))
	     (if (and (integerp a) (integerp b))
		 (funcall fn a b)
		 (list sym a b)))))
    (if (atom expression)
	(cond ((integerp expression) expression)
	      ((stringp expression) (string->int expression))
	      (t (error "Unknown expression atom: ~A" expression)))
	(case (car expression)
	  (+ (bin-op '+ #'+))
	  (- (bin-op '- #'-))
	  (* (bin-op '* #'*))
	  (/ (bin-op '/ #'/))
	  (& (bin-op '& #'logand))
	  (or (bin-op 'or #'logior))
	  (^ (bin-op '^ #'logxor))
	  (<< (bin-op '<< #'ash))
	  (>> (bin-op '<< #'(lambda (n count) (ash n (- count)))))
	  (~ (aif (resolve-expression (second expression))
		  (lognot it)
		  expression))
	  (symbol (aif (get-symbol-value (second expression))
		       it
		       expression))
	  (t expression)))))


;;;; "MAIN" STUFF

;;; XXX should this be maintained by the lexer instead?
(defvar *file-list* nil
  "Stack of file names that are actively open; checked by INCLUDE to
determine whether recursive nesting is occuring.")

(defvar *program-counter*)

(defun assemble (file)
  ;; create symbol table
  (setf *symbol-table* (make-hash-table))
  ;; create object file tmp (write header, start output section)
  ;; init program counter
  (setf *program-counter* 0)
  ;; create backpatch list
  ;; open file and start processing
  (init-lexer file)
  ;; go through backpatch list, try to make all patches
  ;; finalize object file (write symbol table, patch header)
  )

(defun process-stream (stream)
  (handler-case
      (loop
       ;; line-counter
	 ;; we strip individual token position information here, which
	 ;; might suck for superfine debugging/warning precision, but
	 ;; we really don't need all that clutter.
       (multiple-value-bind (line position)
	   (unclutter-line (parse (lambda () (next-token stream))))
	 (assert (eql (pop line) 'line))

	 (awhen (find 'label line :key #'car)
	   (add-to-symbol-table (second it) *program-counter* position))
	 ;; if there's a label here, add it to the symbol table
	 ;;   (get-label)
	 ;; pseudo-ops care about labels, opcodes don't, so
	 ;; strip the label if it's a pseudo-op.

	 ;; separate opcode and operands
	 (let ((operation (cadr (find 'operation line :key #'car)))
	       (operands (mapcar #'simplify-operand 
				 (extract-operands line))))
	   (format t "~&~A ~A => " operation operands)
	   (when (consp operation)
	     (let ((opcode (caadr operation))
		   (modifier (string-to-modifier (cadadr operation))))
	       (if (eql (car operation) 'opcode)
		   (format t "~A"
			   (awhen (find-matching-entry opcode operands modifier)
			     (generate-code (second it) operands modifier)))
		   (format t "~A"
			   (find opcode *asm-pseudo-op-table*
				 :key #'car :test #'string-equal))))))))
    (end-of-file)))

(defun output-data (stream data length)
  "Outputs DATA in big-endian order to the currently active temporary
object file and updates the program counter.  Returns the address to
which the data assembled."
  (unless (zerop (mod length 8))
    (setf length (ash (ceiling (/ length 8)) 3)))
  (do ((pos (- length 8) (- pos 8)))
      ((< pos 0))
    (write-byte (ldb (byte 8 pos) data) stream)))


;;;; EOF assembler.lisp
