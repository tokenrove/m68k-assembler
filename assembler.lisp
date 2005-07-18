
(in-package :m68k-assembler)

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
  (assert (= (length operands) 1))
  (let ((*defining-relocations-p* nil))
    (add-to-symbol-table label (absolute-value (first operands))
			 :type 'absolute)))

(defun execute-macro (name operands modifier)
  (labels ((sub (match &rest registers)
	     (declare (ignore registers))
	     (acase (char match 1)
	       (#\@ (format nil "_~D" 
			    (asm-macro-count (get-symbol-value name))))
	       (#\0 (case modifier (byte ".B") (long ".L") (t ".W")))
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
  (and (> (length name) 1) (eql (char name 0) #\.)))

(defun sacred-name-of-pc-p (name)
  (and (= (length name) 1) (eql (char name 0) #\.)))

(defun maybe-local-label (name)
  (if (local-label-name-p name)
      (concatenate 'string *last-label* name)
      name))

(defun add-to-symbol-table (sym value
			    &key (type (section-name *current-section*))
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
  (if (sacred-name-of-pc-p sym)
      (make-asm-symbol :name "."
		       :value *program-counter*
		       :type (section-name *current-section*))
      (gethash sym *symbol-table*)))

(defun extract-sym-name (sym)
  (let ((name (cond ((atom sym) sym)
			   ((or (eql (car sym) 'absolute)
				(eql (car sym) 'label))
			    (second (second sym)))
			   (t (second sym)))))
    (maybe-local-label name)))

(defun define-extern (label op operands modifier)
  (declare (ignore op modifier))
  ;; add whatever to the symbol table, as an extern
  (handle-label-normally label)
  (dolist (x operands)
    (let ((name (extract-string-from-tree x)))
      (add-to-symbol-table name 0 :type 'extern :global-p t))))

(defun define-global (label op operands modifier)
  (declare (ignore op modifier))
  ;; add whatever to the symbol table, or just tweak
  ;; its type if necessary.
  (handle-label-normally label)
  (dolist (x operands)
    (let ((name (extract-string-from-tree x)))
      (aif (get-asm-symbol name)
	   (setf (asm-symbol-global-p it) t) ; XXX hope this works.
	   (add-to-symbol-table name nil :global-p t)))))

;;;; BACKPATCHING

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *backpatch-list* nil))

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
		  :file-position (file-position
				  (section-object-stream *current-section*))
		  :last-label *last-label*
		  :source-position *source-position*))

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-backpatch ((item) &body body)
    `(let ((*program-counter* (backpatch-program-counter ,item))
	   (*current-section* (backpatch-section ,item))
	   (*last-label* (backpatch-last-label ,item))
	   (*source-position* (backpatch-source-position ,item))
	   (*object-stream* (section-object-stream (backpatch-section ,item))))
      ,@body)))

(defun backpatch ()
  ;; go through backpatch list, try to make all patches
  (dolist (x *backpatch-list*)
    (using-section ((backpatch-section x))
      (with-backpatch (x)
	(let ((*defining-relocations-p* t))
	  (multiple-value-bind (data len)
	      (generate-code (backpatch-template x) nil nil)
	    (when (consp data)
	      (error "~A: Failed to backpatch @ ~A: ~S."
		     *source-position* *program-counter* data))
	    (assert (= len (backpatch-length x)))
	    (assert (file-position (section-object-stream *current-section*)
				   (backpatch-file-position x)))
	    (output-data data len)))))))


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

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *defining-relocations-p* nil))


(defun figure-out-reloc (symbol pc)
  (let* ((sym (get-asm-symbol symbol))
	 (extern-p (eq (asm-symbol-type sym) 'extern)))
    (make-relocation :address pc :extern-p extern-p
		     :symbol (if extern-p
				 (asm-symbol-name sym)
				 (asm-symbol-type sym)))))

(defun check-reloc-consistency (reloc-a reloc-b)
  (assert (every (lambda (fn)
		   (eql (funcall fn reloc-a)
			(funcall fn reloc-b)))
		 (list #'relocation-address
		       ;; XXX relocation-size
		       #'relocation-symbol
		       #'relocation-extern-p))))

(defun add-relocation (symbol)
  (when (and *defining-relocations-p*
	     (not (eq (get-symbol-type symbol) 'absolute)))
    (let ((reloc (figure-out-reloc symbol *program-counter*)))
      (sif (gethash *program-counter* *relocation-table*)
	   (check-reloc-consistency reloc it)
	   (setf it reloc)))))

(defun pc-relativise-relocation ()
  "If there's a relocation at this PC, mark it PC-relative."
  (swhen (gethash *program-counter* *relocation-table*)
    ;; If the symbol is not extern and segment is same as current
    ;; section, then delete from relocation table (no need to relocate
    ;; same-section relative references within the same file).
    (if (or (relocation-extern-p it)
	    (not (eq (relocation-segment it)
		     (section-name *current-section*))))
	(setf (relocation-pc-relative-p it) t)
	(remhash *program-counter* *relocation-table*))))

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
	  (- (if (= (length expression) 2)
		 (let ((v (resolve-expression (second expression))))
		   (if (integerp v) (- v) expression))
		 (bin-op '- #'-)))
	  (* (bin-op '* #'*))
	  (/ (bin-op '/ #'/))
	  (& (bin-op '& #'logand))
	  (or (bin-op 'or #'logior))
	  (^ (bin-op '^ #'logxor))
	  (<< (bin-op '<< #'ash))
	  (>> (bin-op '>> #'(lambda (n count) (ash n (- count)))))
	  (~ (let ((v (resolve-expression (second expression))))
	       (if (integerp v)
		   (lognot v)
		   expression)))
	  (symbol (acond ((get-symbol-value expression)
			  (add-relocation expression)
			  it)
			 (t expression)))
	  ;;; XXX if we got a constant, it's a bug.  That should have
	  ;;; been picked out at the AST stage.
	  (constant (resolve-expression (second expression)))
	  (t expression)))))


;;;; "MAIN" STUFF

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *source-position*)
  (defvar *last-label* nil "Last label seen by the assembler.  This is
used for generating the prefix for local labels.")
  (defvar *current-section* nil "Current section we're assembling
in."))


(defun assemble (input-name &key (object-name "mr-ed.o")
		 (object-format :aout))
  "Assembles a source file named by INPUT-NAME, to an object file
named by OBJECT-NAME.

OBJECT-FORMAT can currently only be :AOUT for relocatable a.out object
files."
  ;; create symbol table
  (setf *symbol-table* (make-hash-table :test 'equal)
	;; create backpatch list
	*backpatch-list* nil
	;; init macro variables
	*defining-macro-p* nil *defining-rept-p* nil
	;; last label seen
	*last-label* nil
	*sections* nil)
  ;; open file and start processing
  (with-lexer (input-name)
    (with-sections ('text 'data 'bss)
      (setf *current-section* (cdr (assoc 'text *sections*)))
      (process-current-file)
      ;; Prior to backpatching, record section lengths, because
      ;; temporary-files aren't really file streams the way CL would
      ;; like them to be, so we can't just FILE-LENGTH them.
      (let ((lengths (mapcar (lambda (x)
			       (cons (car x)
				     (section-length (cdr x))))
			     *sections*)))
	(backpatch)
	(ecase object-format
	  (:aout (finalize-object-file object-name lengths)))))))



(defun process-current-file ()
  (handler-case
      (loop
       ;; we strip individual token position information here, which
       ;; might suck for superfine debugging/warning precision, but
       ;; we really don't need all that clutter.
       (multiple-value-bind (line *source-position*)
	   (unclutter-line (parse #'next-token))
	 (assert (eq (pop line) 'line) ()
		 "Some internal parse tree is messed up.")

	 (using-section (*current-section*)
	   ;; if we're in a macro or repeat, just accumulate this
	   ;; line.  otherwise, process it.
	   (cond ((or *defining-macro-p* *defining-rept-p*)
		  (if (and (eq (operation-type-of-line line) 'pseudo-op)
			   (string-equal (opcode-of-line line)
					 (if *defining-macro-p*
					     "ENDM" "ENDR")))
		      (process-line line)
		      (push line *macro-buffer*)))
		 (t (process-line line))))))
    (end-of-file nil)))

(defun operation-type-of-line (line)
  (awhen (cadr (find 'operation line :key #'car)) (car it)))

(defun opcode-of-line (line)
  (awhen (cadr (find 'operation line :key #'car)) (caadr it)))

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
	   (handle-label-normally label)))))

(defun handle-label-normally (label)
  (when label
    (let ((name (extract-string-from-tree label)))
      (when (not (local-label-name-p name))
	(setf *last-label* name))
      ;; If the type is nil, the symbol isn't present.
      (cond ((null (get-symbol-type name))
	     (add-to-symbol-table name *program-counter*))
	    ;; If the value is nil, it was probably declared as a global
	    ;; before it was actually defined.  Patch value, debug-info.
	    ((null (get-symbol-value name))
	     (let ((sym (get-asm-symbol name)))
	       (assert (not (eq (asm-symbol-type sym) 'extern))
		       (name) "Trying to set a value for an EXTERN symbol!")
	       (setf (asm-symbol-value sym) *program-counter*
		     (asm-symbol-debug-info sym) *source-position*)))
	    ;; Otherwise, it's a redefinition and I won't stand for it.
	    (t (warn "~A: tried to redefine label ~A -- ignoring."
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
    (let ((*defining-relocations-p* t))
      (acond ((find opcode *asm-pseudo-op-table*
		    :key #'car :test #'string-equal)
	      (when (functionp (second it))
		(funcall (second it) label opcode operands modifier)))
	     ((eql (get-symbol-type opcode) 'macro)
	      (execute-macro opcode operands modifier))
	     (t (error "~&~S: bad pseudo-op ~A!"
		       *source-position* opcode))))))


(defun output-data (data length)
  "Outputs DATA in big-endian order to the currently active temporary
object file and updates the program counter.  Returns the address to
which the data assembled."
  #+nil (unless (zerop (mod length 8))
    (setf length (ash (ceiling (/ length 8)) 3)))
  (when (eq (section-name *current-section*) 'text)
    (unless (zerop (mod length 16))
      (format t "~&~A: ~A" *source-position* length)))
  (incf *program-counter* (ash length -3))
  (funcall (section-output-fn *current-section*)
	   *object-stream* data length))


;;;; EOF assembler.lisp
