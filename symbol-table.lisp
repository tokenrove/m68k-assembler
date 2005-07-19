(in-package :m68k-assembler)

;;;; SYMBOL TABLE

(defvar *symbol-table* nil)

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

(defun maybe-local-label (name last-label)
  (if (local-label-name-p name)
      (concatenate 'string last-label name)
      name))

(defun wipe-symbol-table ()
  (setf *symbol-table* (make-hash-table :test #'equalp)))

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
    (maybe-local-label name *last-label*)))

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

(defun serialize-symbol-table ()
  (let ((table (make-array (list 0) :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (when (asm-symbol-global-p v)
		 (vector-push-extend v table)))
	     *symbol-table*)
    table))