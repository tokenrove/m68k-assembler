(in-package :m68k-assembler)

;;;; PSEUDO-OPS

(defun pseudo-op-p (string)
  (or (get-pseudo-op string) (eql (get-symbol-type string) 'macro)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((asm-pseudo-op-table (make-hash-table :test 'equalp)))
    (dolist (x `(("SECTION" 
		  ,(lambda (label op operands modifier)
		    (declare (ignore op modifier))
		    (handle-label-normally label)
		    (assert (and (eql (caar operands) 'absolute)
				 (eql (caadar operands) 'symbol)))
		    (assert (= (length operands) 1))
		    (let ((section (intern (string-upcase
					    (cadar (cdar operands)))
					   (find-package "M68K-ASSEMBLER"))))
		      (setf *current-section* (cdr (assoc section *sections*)))
		      (assert *current-section*))))
		 ("XDEF" ,#'define-global)
		 ("GLOBAL" ,#'define-global)
		 ("XREF" ,#'define-extern)
		 ("EXTERN" ,#'define-extern)
		 ("ORG" ,(lambda (label op operands modifier)
			  (declare (ignore label op modifier))
			  (assert (eql (car (first operands)) 'absolute))
			  (assert (= (length operands) 1))
			  (setf *program-counter* (absolute-value (first operands)))))

		 ("INCLUDE"
		  ,(lambda (label op operands modifier)
		    (declare (ignore op modifier))
		    (assert (= (length operands) 1))
		    (handle-label-normally label)
		    (nested-lexing (extract-string-from-tree (first operands)))))
		 ("INCBIN"
		  ,(lambda (label op operands modifier)
		    (declare (ignore op modifier))
		    (assert (= (length operands) 1))
		    (handle-label-normally label)
		    (with-open-file (stream (extract-string-from-tree
					     (first operands))
					    :direction :input
					    :element-type 'unsigned-byte)
		      (copy-stream-contents stream
					    (section-object-stream
					     *current-section*))
		      (incf *program-counter* (file-position stream)))))

		 ("ALIGN" ,(lambda (label op operands modifier)
			    (declare (ignore op modifier))
			    (assert (= (length operands) 1))
			    (handle-label-normally label)
			    (let ((align (absolute-value (first operands))))
			      (do ()
				  ((zerop (mod *program-counter* align)))
				(output-data 0 8)))))
		 ("EVEN" ,(lambda (label op operands modifier)
			   (declare (ignore op modifier operands))
			   (handle-label-normally label)
			   (unless (evenp *program-counter*)
			     (output-data 0 8))))
		 ;; offsetalign -- offset+(PC+align-1)&~(align-1)
		 ;; XXX untested.
		 ("CNOP" ,(lambda (label op operands modifier)
			   (declare (ignore op modifier))
			   (handle-label-normally label)
			   (let ((offset (absolute-value (first operands)))
				 (align (absolute-value (second operands))))
			     (do ()
				 ((zerop (mod (- *program-counter* offset) align)))
			       (output-data 0 8)))))

		 ("MACRO" ,#'start-macro)
		 ("ENDM" ,#'end-macro)

		 ("REPT"
		  ,(lambda (label op operands modifier)
		    (declare (ignore label op modifier))
		    (assert (not *defining-rept-p*))
		    (assert (= (length operands) 1))
		    (setf *macro-buffer* (list (absolute-value (first operands)))
			  *defining-rept-p* t)))
		 ("ENDR"
		  ,(lambda (label op operands modifier)
		    (declare (ignore label op operands modifier))
		    (assert *defining-rept-p*)
		    (setf *macro-buffer* (nreverse *macro-buffer*)
			  *defining-rept-p* nil)
		    (dotimes (i (pop *macro-buffer*))
		      (dolist (x *macro-buffer*)
			(process-line x)))))

		 ("DC"
		  ,(lambda (label op operands modifier)
		    (declare (ignore op))
		    (handle-label-normally label)
		    (unless modifier (setf modifier 'word))
		    (dolist (x operands)
		      (let ((data (absolute-value x modifier))
			    (length (ecase modifier (byte 8) (word 16) (long 32))))
			(when (consp data)
			  (push (make-backpatch-item `((,length (absolute-value ,data ,modifier)))
						     length)
				*backpatch-list*)
			  (setf data 0))
			;; The behavior here is that if we're asked to DC
			;; some constant larger than the length we output
			;; it in length chunks.  This might be undesired
			;; behavior for some modifiers although it's
			;; almost certainly desired behavior for bytes.
			;; Maybe some heuristics and warnings should go
			;; here or at least a flag to enable/disable this
			;; behavior.
			(when (minusp data) ; XXX this is totally broken!
			  (setf data (+ (ash 1 length) data)))
			(do ((total (if (<= 0 data 1) 1 (ceiling (log data 2)
								 length))
				    (1- total)))
			    ((<= total 0))
			  (output-data (ldb (byte length (* (1- total) length))
					    data)
				       length))))))
		 ("DS"
		  ,(lambda (label op operands modifier)
		    (declare (ignore op))
		    (handle-label-normally label)
		    (unless modifier (setf modifier 'word))
		    (assert (eql (car (first operands)) 'absolute))
		    (assert (= (length operands) 1)) ; XXX probably not.
		    (let ((data (absolute-value (first operands) modifier))
			  (length (ecase modifier (byte 8) (word 16) (long 32))))
		      (unless (integerp data)
			(error "~A: Need to be able to resolve DS immediately."
			       *source-position*))
		      (output-data 0 (* data length)))))
		 ;; ("DCB")	      ; constant block -- numbervalue

		 ("EQU" ,#'define-equate)
		 ("=" ,#'define-equate)
		 ;; EQUR (register equate)

		 ;; Conditional compilation.
		 ;; XXX What other conditions are there?
		 ("IFEQ"
		  ,(lambda (label op operands modifier)
		    (declare (ignore label op modifier))
		    (assert (not *defining-conditional-compilation-p*))
		    (assert (= (length operands) 1))
		    (setf *macro-buffer* (list (zerop (absolute-value (first operands))))
			  *defining-conditional-compilation-p* t)))
		 ("IFNE"
		  ,(lambda (label op operands modifier)
		    (declare (ignore label op modifier))
		    (assert (not *defining-conditional-compilation-p*))
		    (assert (= (length operands) 1))
		    (setf *macro-buffer* (list (/= 0 (absolute-value (first operands))))
			  *defining-conditional-compilation-p* t)))
		 ("ENDC"
		  ,(lambda (label op operands modifier)
		    (declare (ignore label op operands modifier))
		    (assert *defining-conditional-compilation-p*)
		    (setf *macro-buffer* (nreverse *macro-buffer*)
			  *defining-conditional-compilation-p* nil)
		    (when (pop *macro-buffer*)
		      (dolist (x *macro-buffer*)
			(process-line x)))))

		 ;; ("END")  ; early end of source.
		 ))
      (setf (gethash (first x) asm-pseudo-op-table) (second x)))
    (defun get-pseudo-op (name) 
      (gethash name asm-pseudo-op-table))))
