
(in-package :m68k-assembler)

;;;; PSEUDO-OPS

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *asm-pseudo-op-table*
    `(("SECTION" ,(lambda (label op operands modifier)
			  (declare (ignore op modifier))
			  (handle-label-normally label)
			  (assert (and (eql (caar operands) 'absolute)
				       (eql (caadar operands) 'symbol)))
			  (let ((section (intern (cadar (cdar operands))
						 (find-package "M68K-ASSEMBLER"))))
			    (assert (assoc section *object-streams*))
			    (setf *current-section* section))))
      ("XDEF" #'define-global)
      ("GLOBAL" #'define-global)
      ("XREF" #'define-extern)
      ("EXTERN" #'define-extern)
      ("ORG" ,(lambda (label op operands modifier)
		      (declare (ignore label op modifier))
		      (assert (eql (car (first operands)) 'absolute))
		      (setf *program-counter* (absolute-value (first operands)))))

      ("INCLUDE"
       ,(lambda (label op operands modifier)
		(declare (ignore op modifier))
		(handle-label-normally label)
		(nested-lexing (extract-string-from-tree (first operands)))))
      ("INCBIN"
       ,(lambda (label op operands modifier)
		(declare (ignore op modifier))
		(handle-label-normally label)
		(with-open-file (stream (extract-string-from-tree
					 (first operands))
					:direction :input
					:element-type 'unsigned-byte)
		  (copy-stream-contents stream (current-obj-stream))
		  (incf *program-counter* (file-position stream)))))

      ("ALIGN" ,(lambda (label op operands modifier)
			(declare (ignore op modifier))
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
      ;; offset,align -- offset+(PC+align-1)&~(align-1)
      ;; XXX untested.
      ("CNOP" ,(lambda (label op operands modifier)
		       (declare (ignore op modifier))
		       (handle-label-normally label)
		       (let ((offset (absolute-value (first operands)))
			     (align (absolute-value (second operands))))
			 (do ()
			     ((zerop (mod (- *program-counter* offset) align)))
			   (output-data 0 8)))))

      ("MACRO"
       ,(lambda (label op operands modifier)
		(declare (ignore op operands modifier))
		;; if *defining-macro-p* is already set, cry foul.
		(assert (not *defining-macro-p*))
		(assert label)
		;; otherwise, clear out *macro-buffer*.
		(setf *macro-buffer* (list (second label))
		      *defining-macro-p* t)))
      ("ENDM"
       ,(lambda (label op operands modifier)
		(declare (ignore label op operands modifier))
		(assert *defining-macro-p*)
		(setf *macro-buffer* (nreverse *macro-buffer*)
		      *defining-macro-p* nil)
		(add-to-symbol-table (first *macro-buffer*)
				     (make-asm-macro :body (cdr *macro-buffer*))
				     :type 'macro)))

      ("REPT"
       ,(lambda (label op operands modifier)
		(declare (ignore label op modifier))
		(assert (not *defining-rept-p*))
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
		      (push (make-backpatch-item
			     `((,length (absolute-value ,data ,modifier)))
			     length)
			    *backpatch-list*)
		      (setf data #x4E714E71))
		    (output-data data length)))))
      ("DS"
       ,(lambda (label op operands modifier)
		(declare (ignore op))
		(handle-label-normally label)
		(unless modifier (setf modifier 'word))
		(assert (eql (car (first operands)) 'absolute))
		(let ((data (absolute-value (first operands) modifier))
		      (length (ecase modifier (byte 8) (word 16) (long 32))))
		  (unless (integerp data)
		    (error "~A: Need to be able to resolve DS immediately."
			   *source-position*))
		  (output-data 0 (* data length)))))
      ("DCB")			      ; constant block -- number,value

      ("EQU" #'define-equate)
      ("=" #'define-equate)
      ;; EQUR (register equate)
      ;; IFEQ etc etc

      ("END"))))

(defun pseudo-op-p (string)
  (or (find string *asm-pseudo-op-table* :key #'car :test #'string-equal)
      (eql (get-symbol-type string) 'macro)))


;;;; MACROS

;; Devpac macros -- when we see a macro start, collect until the ENDM,
;; so we can expand ourselves.

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *defining-macro-p* nil)
  (defvar *defining-rept-p* nil)
  (defvar *macro-buffer*))

(defstruct asm-macro
  (count 0)
  (body))

(defun define-equate (label op operands modifier)
  (declare (ignore op modifier))
  (unless label
    (error "~A: EQU always needs a label." *source-position*))
  ;; XXX should probably check operands length etc
  (add-to-symbol-table label (resolve-expression (first operands))
		       :type 'absolute))

(defun execute-macro (name operands modifier)
  (labels ((sub (match &rest registers)
	       (declare (ignore registers))
	       (acase (char match 1)
		 (#\@ (format nil "_~D" 
			      (asm-macro-count (get-symbol-value name))))
		 (#\0 (case modifier
			(byte ".B")
			(long ".L")
			((word t) ".W")))
		 (t (nth (digit-to-int it 36) operands))))
	     (seek+destroy (tree)
	       (cond ((stringp tree)
		      (cl-ppcre:regex-replace-all "\\\\[0-9A-Za-z@]"
						  tree #'sub
						  :simple-calls t))
		     ((consp tree)
		      (let ((new-tree nil))
			(dolist (branch tree)
			  (push (seek+destroy branch) new-tree))
			(nreverse new-tree)))
		     (t tree))))
      (dolist (x (asm-macro-body (get-symbol-value name)))
	(process-line (seek+destroy x)))
      (incf (asm-macro-count (get-symbol-value name)))))


;;;; SYMBOL TABLE

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *symbol-table* nil))

(defstruct asm-symbol
  (name)
  (type)
  (value)
  (debug-info)
  (global-p nil))

(defun local-label-name-p (name)
  (and (plusp (length name)) (eql (char name 0) #\.)))

(defun maybe-local-label (name)
  (if (local-label-name-p name)
      (concatenate 'string *last-label* name)
      name))

(defun add-to-symbol-table (sym value &key (type *current-section*)
			    (global-p nil))
  (let ((name (extract-sym-name sym)))
    (setf (gethash name *symbol-table*)
	  (make-asm-symbol :name name
			   :type type
			   :value value
			   :debug-info *source-position*
			   :global-p global-p))))

(defun get-symbol-value (sym)
  (awhen (get-asm-symbol sym) (asm-symbol-value it)))

(defun (setf get-symbol-value) (value sym)
  (setf (asm-symbol-value (get-asm-symbol sym)) value))

(defun get-symbol-type (sym)
  (awhen (get-asm-symbol sym) (asm-symbol-type it)))

(defun get-asm-symbol (sym)
  (setf sym (extract-sym-name sym))
  (gethash sym *symbol-table*))

(defun extract-sym-name (sym)
  (maybe-local-label (cond ((atom sym) sym)
			   ((or (eql (car sym) 'absolute)
				(eql (car sym) 'label))
			    (second (second sym)))
			   (t (second sym)))))

(defun define-extern (label op operands modifier)
  (declare (ignore op modifier))
  ;; add whatever to the symbol table, as an extern
  (handle-label-normally label)
  (let ((name (extract-string-from-tree (first operands))))
    (add-to-symbol-table name nil :type 'extern :global-p t)))

(defun define-global (label op operands modifier)
  (declare (ignore op modifier))
  ;; add whatever to the symbol table, or just tweak
  ;; its type if necessary.
  (handle-label-normally label)
  (let ((name (extract-string-from-tree (first operands))))
    (aif (get-asm-symbol name)
	 (setf (asm-symbol-global-p it) t) ; XXX hope this works.
	 (add-to-symbol-table name nil :global-p t))))

;;;; BACKPATCHING

(defvar *backpatch-list* nil)

;; backpatch structure
(defstruct backpatch
  (template)
  (length)
  (program-counter)
  (section)
  (file-position)
  (last-label)
  (source-position))

(defun make-backpatch-item (data length)
  (make-backpatch :template data :length length
		  :program-counter *program-counter*
		  :section *current-section*
		  :file-position (file-position (current-obj-stream))
		  :last-label *last-label*
		  :source-position *source-position*))

(defmacro with-backpatch ((item) &body body)
  `(let ((*program-counter* (backpatch-program-counter ,item))
	 (*current-section* (backpatch-section ,item))
	 (*last-label* (backpatch-last-label ,item))
	 (*source-position* (backpatch-source-position ,item)))
    ,@body))

(defun backpatch ()
  ;; go through backpatch list, try to make all patches
  (dolist (x *backpatch-list*)
    (with-backpatch (x)
      (let ((*defining-relocations-p* t))
	(multiple-value-bind (data len)
	    (generate-code (backpatch-template x) nil nil)
	  (when (consp data)
	    (error "~A: Failed to backpatch @ ~A: ~S."
		   *source-position* *program-counter* data))
	  (assert (= len (backpatch-length x)))
	  (assert (file-position (current-obj-stream)
				 (backpatch-file-position x)))
	  (output-data data len))))))


;;;; RELOCATION

;; relocation info
(defstruct relocation
  (address)
  (size nil)
  (extern-p nil)
  (pc-relative-p nil)
  ;; only one of symbol or segment can be selected at a time, so the
  ;; symbol field provides both functions.
  (symbol))

(defun relocation-segment (r) (relocation-symbol r))
(defun (setf relocation-segment) (v r) (setf (relocation-segment r) v))

;; Indexed by PC.
(defvar *relocation-table*)
(defvar *defining-relocations-p* nil)


(defun figure-out-reloc (symbol pc)
  (let* ((sym (get-asm-symbol symbol))
	 (extern-p (eq (asm-symbol-type sym) 'extern)))
    (make-relocation :address pc :extern-p extern-p
		     :symbol (if extern-p symbol (asm-symbol-type sym)))))

(defun check-reloc-consistency (reloc-a reloc-b)
  (assert (every (lambda (fn)
		   (eql (funcall fn reloc-a)
			(funcall fn reloc-b)))
		 (list #'relocation-address
		       ;; XXX relocation-size
		       #'relocation-symbol
		       #'relocation-extern-p))))

(defun add-relocation (symbol)
  (when *defining-relocations-p*
    ;; figure out what kind of relocation this is
    (let ((reloc (figure-out-reloc symbol *program-counter*)))
      (sif (gethash *program-counter* *relocation-table*)
	   (check-reloc-consistency reloc it)
	   (setf it reloc)))))

(defun pc-relativise-relocation ()
  ;; if there's a reloc at this PC, make it pc-relative.
  ;; XXX if the symbol is not extern, then delete from relocation
  ;; table (no need to relocate relative references within the same
  ;; file).
  (swhen (gethash *program-counter* *relocation-table*)
    (setf (relocation-pc-relative-p it) t)))

(defun fix-relocation-size (size)
  (swhen (gethash *program-counter* *relocation-table*)
    (setf (relocation-size it) size)))


;;;; HELPER FUNCTIONS

(defun extract-string-from-tree (tree)
  "Return the first string we come to in TREE."
  (dolist (x tree)
    (cond ((stringp x) (return x))
	  ((consp x)
	   (awhen (extract-string-from-tree x)
	     (return it))))))

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
	  (>> (bin-op '>> #'(lambda (n count) (ash n (- count)))))
	  (~ (aif (resolve-expression (second expression))
		  (lognot it)
		  expression))
	  (symbol (aif (get-symbol-value expression)
		       (progn 
			 (add-relocation expression)
			 it)
		       expression))
	  (t expression)))))


;;;; "MAIN" STUFF

(defvar *source-position*)
(defvar *object-streams*)
(defvar *program-counter*)
(defvar *last-label* nil "Last label seen by the assembler.  This is
used for generating the prefix for local labels.")
(defvar *current-section* 'text "Current section we're assembling
in.")

(defmacro with-object-streams (sections &body body)
  (if sections
      (let ((symbol-of-the-day (gensym)))
	`(osicat:with-temporary-file (,symbol-of-the-day
				      :element-type 'unsigned-byte)
	  (push (cons ,(car sections) ,symbol-of-the-day) *object-streams*)
	  (with-object-streams ,(cdr sections) ,@body)))
      `(progn ,@body)))


(defun current-obj-stream ()
  (cdr (assoc *current-section* *object-streams*)))

(defun assemble (input-name &key (object-name "mr-ed.o"))
  ;; create symbol table
  (setf *symbol-table* (make-hash-table :test 'equal)
	;; init program counter
	*program-counter* 0
	;; create backpatch list
	*backpatch-list* nil
	;; init macro variables
	*defining-macro-p* nil *defining-rept-p* nil
	;; last label seen
	*last-label* nil
	*current-section* 'text
	*object-streams* nil
	*relocation-table* (make-hash-table))
  ;; open file and start processing
  (with-lexer (input-name)
    ;; Note that we create a file for BSS even though it should all be
    ;; zeros just for my utterly lazy convenience.  It's wasteful, but
    ;; I don't feel like putting in all kinds of hideous special cases
    ;; just right now.
    (with-object-streams ('text 'data 'bss)
      (process-current-file)
      ;; Prior to backpatching, record section lengths, because
      ;; temporary-files aren't really file streams the way CL would
      ;; like them to be, so we can't just FILE-LENGTH them.
      (let ((lengths (mapcar (lambda (x)
			       (cons (car x) (file-position (cdr x))))
			     *object-streams*)))
	(backpatch)
	(finalize-object-file object-name lengths)))))

(defun process-current-file ()
  (handler-case
      (loop
       ;; we strip individual token position information here, which
       ;; might suck for superfine debugging/warning precision, but
       ;; we really don't need all that clutter.
       (multiple-value-bind (line *source-position*)
	   (unclutter-line (parse #'next-token))
	 (assert (eql (pop line) 'line))

	 ;; if we're in a macro or repeat, accumulate this line.
	 (cond (*defining-macro-p*
		(if (and (eql (operation-type-of-line line) 'pseudo-op)
			 (string-equal (opcode-of-line line) "ENDM"))
		    (process-line line)
		    (push line *macro-buffer*)))
	       (*defining-rept-p*
		(if (and (eql (operation-type-of-line line) 'pseudo-op)
			 (string-equal (opcode-of-line line) "ENDR"))
		    (process-line line)
		    (push line *macro-buffer*)))
	       (t (process-line line))))) ; otherwise, process it.
    (end-of-file nil)))

(defun operation-type-of-line (line)
  (awhen (cadr (find 'operation line :key #'car))
    (car it)))

(defun opcode-of-line (line)
  (awhen (cadr (find 'operation line :key #'car))
    (caadr it)))

(defun process-line (line)
  (let ((label (find 'label line :key #'car))
	(operation (cadr (find 'operation line :key #'car)))
	(operands (mapcar #'simplify-operand 
			  (extract-operands line))))
    (cond ((eql (operation-type-of-line line) 'opcode)
	   (assemble-opcode label operation operands))
	  ((eql (operation-type-of-line line) 'pseudo-op)
	   (assemble-pseudo-op label operation operands))
	  (t
	   (when label
	     ;; might be a macro or something... see if it
	     ;; exists, first.  and complain about
	     ;; redefinition, except in the case of locals.
	     (if (get-symbol-type label)
		 (warn "~&~S: tried to redefine ~A (cowardly not allowing this)." *source-position* label)
		 (handle-label-normally label)))))))

(defun handle-label-normally (label)
  (when label
    (let ((name (extract-string-from-tree label)))
      (when (not (local-label-name-p name))
	(setf *last-label* name))
      ;; If the type is nil, the symbol isn't present.
      (cond ((null (get-symbol-type name))
	     (add-to-symbol-table name *program-counter*))
	    ;; If the value is nil, it was probably declared as a global
	    ;; before it was actually defined.
	    ((null (get-symbol-value name))
	     (assert (not (eq (get-symbol-type name) 'extern))
		     (name) "Trying to set a value for an EXTERN symbol!")
	     (setf (get-symbol-value name) *program-counter*))
	    ;; Otherwise, it's a redefinition and I won't stand for it.
	    (t (warn "~A: tried to redefine label ~A"
		     *source-position* name))))))

(defun assemble-opcode (label operation operands)
  (let ((opcode (caadr operation))
	(modifier (cadadr operation)))
    (handle-label-normally label)
    (awhen (find-matching-entry opcode operands modifier)
      (let ((*defining-relocations-p* t))
	(multiple-value-bind (data len)
	    (generate-code (second it) operands modifier)
	  (when (consp data)
	    (push (make-backpatch-item data len) *backpatch-list*)
	    (setf data #x4E714E71))	; Output NOP if all else fails.
	  (output-data data len))))))


(defun assemble-pseudo-op (label operation operands)
  (let ((opcode (caadr operation))
	(modifier (cadadr operation)))
    (acond ((find opcode *asm-pseudo-op-table*
		  :key #'car :test #'string-equal)
	    (when (functionp (second it))
	      (funcall (second it) label opcode operands modifier)))
	   ((eql (get-symbol-type opcode) 'macro)
	    (execute-macro opcode operands modifier))
	   (t (error "~&~S: bad pseudo-op ~A!" *source-position* opcode)))))


(defun output-data (data length)
  "Outputs DATA in big-endian order to the currently active temporary
object file and updates the program counter.  Returns the address to
which the data assembled."
  #+nil (unless (zerop (mod length 8))
    (setf length (ash (ceiling (/ length 8)) 3)))
  (incf *program-counter* (ash length -3))
  (write-big-endian-data (current-obj-stream) data length))


;;;; EOF assembler.lisp
