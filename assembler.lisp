
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

;;;; HELPER FUNCTIONS

(defun munge-modifier-and-find-in-table (string table)
  (let ((modifier nil))
    (awhen (position #\. string)
      (setf modifier (subseq string (1+ it)))
      (setf string (subseq string 0 it)))
    (awhen (find string table :key #'car :test #'string-equal)
      (list string modifier))))

(defun register-p (string)
  (when (string-equal string "sp") (setf string "a7"))
  (munge-modifier-and-find-in-table string *asm-register-table*))

(defun opcode-p (string)
  (munge-modifier-and-find-in-table string *asm-opcode-table*))

(defun pseudo-op-p (string)
  (munge-modifier-and-find-in-table string *asm-pseudo-op-table*))

(defun string-to-modifier (string)
  (cond ((string-equal string "b") 'byte)
	((string-equal string "w") 'word)
	((string-equal string "l") 'long)))


;;;; "MAIN" STUFF

;;; XXX should this be maintained by the lexer instead?
(defvar *file-list* nil
  "Stack of file names that are actively open; checked by INCLUDE to
determine whether recursive nesting is occuring.")

(defun assemble (file)
  ;; create symbol table
  ;; create object file tmp (write header, start output section)
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
       (let ((line (parse (lambda () (next-token stream)))))
	 (assert (eql (pop line) 'line))
	 ;; we strip individual token position information here, which
	 ;; might suck for superfine debugging/warning precision, but
	 ;; we really don't need all that clutter.
	 ; (let ((position (extract-position-info line)))
	 ;   (strip-position-info line)

	 ;; if there's a label here, add it to the symbol table
	 ;;   (get-label)
	 ;; pseudo-ops care about labels, opcodes don't, so
	 ;; strip the label if it's a pseudo-op.

	 ;; separate opcode and operands
	 (format t "~&~%~A " (cadr (find 'operation line :key #'car)))
	 (dolist (x (mapcar #'simplify-operands
			    (extract-operands line)))
	   (format t "~&~A " x))))
    (end-of-file)))

(defun output-data (data)
  "Outputs DATA to the currently active temporary object file and
updates the program counter.  Returns the address to which the data
assembled.")
