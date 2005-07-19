(in-package :m68k-assembler)

;;;; SECTIONS

(defstruct section
  (name)
  (object-stream)
  (output-fn #'write-big-endian-data)
  (relocations (make-hash-table))
  (program-counter 0))


(defun section-length (section)
  #+nil
  (unless (eq (section-name section) 'bss)
    (assert (= (section-program-counter section)
	       (file-position (section-object-stream section)))))
  ;; XXX broken for ORG
  (section-program-counter section))


(defmacro with-sections (sections &body body)
  "Creates the sections named in the SECTIONS list, and executes BODY
with *SECTIONS* bound to an alist containing the sections.  Except in
the case of BSS, a temporary stream is created for the object file
output to a given section."
  (if sections
      (if (eq (car sections) 'bss)
	  `(progn
	    (push (cons ,(car sections)
		   (make-section :name ,(car sections)
				 :output-fn #'bss-output-fn
				 :object-stream nil
				 :relocations nil))
	     *sections*)
	    (with-sections ,(cdr sections) ,@body))

	  (let ((symbol-of-the-day (gensym)))
	    `(osicat:with-temporary-file (,symbol-of-the-day
					  :element-type 'unsigned-byte)
	      (push (cons ,(car sections)
		     (make-section :name ,(car sections)
				   :object-stream ,symbol-of-the-day))
	       *sections*)
	      (with-sections ,(cdr sections) ,@body))))

      `(progn ,@body)))


(defmacro using-section ((section) &body body)
  "Binds various special variables (*PROGRAM-COUNTER*,
*RELOCATION-TABLE*, *OBJECT-STREAM*) to the environment given by
SECTION."
  (let ((temporary (gensym)))
    `(let* ((,temporary ,section)
	    (*program-counter* (section-program-counter ,temporary))
	    (*relocation-table* (section-relocations ,temporary))
	    (*object-stream* (section-object-stream ,temporary)))
      (unwind-protect
	   (progn ,@body)
	(setf (section-program-counter ,temporary) *program-counter*)))))