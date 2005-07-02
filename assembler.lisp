
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
  (when (consp sym)
    (setf sym (second sym)))
  (awhen (gethash sym *symbol-table*) (first it)))

;;;; BACKPATCHING

(defvar *backpatch-list* nil)

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
	  (symbol (aif (get-symbol-value expression)
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
  (setf *symbol-table* (make-hash-table :test 'equal))
  ;; init program counter
  (setf *program-counter* 0)
  ;; create backpatch list
  (setf *backpatch-list* nil)
  ;; open file and start processing
  (init-lexer file)
  (with-open-file (in-stream file)
    ;; create object file tmp (write header, start output section)
    (with-open-file (obj-stream "foo.bin" :direction :output
				:element-type 'unsigned-byte
				:if-exists :overwrite
				:if-does-not-exist :create)
      (process-file in-stream obj-stream)

      (maphash (lambda (k v) (format t "~&~A => ~A" k v))
	       *symbol-table*)
      ;; go through backpatch list, try to make all patches
      (dolist (x *backpatch-list*)
	(destructuring-bind (template length *program-counter*
			     file-position src-position) x
	  (multiple-value-bind (data len) (generate-code template nil nil)
	    (when (consp data)
	      (error "Failed to backpatch @ ~A/~A: ~S."
		     *program-counter* src-position data))
	    (assert (= len length))
	    (assert (file-position obj-stream file-position))
	    (output-data obj-stream data len))))
      ;; finalize object file (write symbol table, patch header)

      )))

(defun process-file (in-stream obj-stream)
  (handler-case
      (loop
       ;; line-counter
	 ;; we strip individual token position information here, which
	 ;; might suck for superfine debugging/warning precision, but
	 ;; we really don't need all that clutter.
       (multiple-value-bind (line position)
	   (unclutter-line (parse (lambda () (next-token in-stream))))
	 (assert (eql (pop line) 'line))
       	 (let ((label (find 'label line :key #'car))
	       (operation (cadr (find 'operation line :key #'car)))
	       (operands (mapcar #'simplify-operand 
				 (extract-operands line))))
	   (cond ((and (consp operation) (eql (car operation) 'opcode))
		  (assemble-opcode label operation operands obj-stream position))
		 ((and (consp operation) (eql (car operation) 'pseudo-op))
		  (assemble-pseudo-op label operation operands obj-stream position))
		 (t (when label
		      ;; might be a macro or an equate... see if it
		      ;; exists, first.  and complain about
		      ;; redefinition, except in the case of locals.
		      (add-to-symbol-table (second label) *program-counter*
					   position)))))))
    (end-of-file)))

(defun assemble-opcode (label operation operands obj-stream position)
  (let ((opcode (caadr operation))
	(modifier (string-to-modifier (cadadr operation))))
    (when label
      (add-to-symbol-table (second label)
			   *program-counter* position))
    (awhen (find-matching-entry opcode operands modifier)
      (multiple-value-bind (data len)
	  (generate-code (second it) operands modifier)
	(when (consp data)
	  (push (list data len *program-counter*
		      (file-position obj-stream) position)
		*backpatch-list*)
	  (setf data #x4E714E71))	; Output NOP if all else fails.
	(output-data obj-stream data len)))))


(defun assemble-pseudo-op (label operation operands obj-stream position)
  (let ((opcode (caadr operation))
	(modifier (string-to-modifier (cadadr operation))))
    (find opcode *asm-pseudo-op-table*
	  :key #'car :test #'string-equal)
    ;; (do-pseudo-op whatever label operands)
    ))


(defun output-data (stream data length)
  "Outputs DATA in big-endian order to the currently active temporary
object file and updates the program counter.  Returns the address to
which the data assembled."
  (unless (zerop (mod length 8))
    (setf length (ash (ceiling (/ length 8)) 3)))
  (incf *program-counter* (ash length -3))
  (do ((pos (- length 8) (- pos 8)))
      ((< pos 0))
    (write-byte (ldb (byte 8 pos) data) stream)))


;;;; EOF assembler.lisp
