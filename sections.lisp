(in-package :m68k-assembler)

;;;; SECTIONS

(defstruct section
  (name)
  (object-stream)
  (output-fn #'write-big-endian-data)
  (relocations (make-hash-table))
  (program-counter 0))


(defun section-length (section)
  ;; XXX broken for ORG; perhaps we should always defer org to the
  ;; linker? -JS
  (section-program-counter section))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-bss (name)
    `(push (cons ,name (make-section :name ,name
			:output-fn #'bss-output-fn
			:object-stream nil
			:relocations nil))
      *sections*))
  (defun create-stream-section (name stream)
    `(push (cons ,name
	    (make-section :name ,name
	     :object-stream ,stream))
      *sections*)))

(defmacro with-sections (sections &body body)
  "Creates the sections named in the SECTIONS list, and executes BODY
with *SECTIONS* bound to an alist containing the sections.  Except in
the case of BSS, a temporary stream is created for the object file
output to a given section."
  (cond ((null sections) `(progn ,@body))
	((eq (car sections) 'bss)
	 `(progn
	   ,(create-bss sections)
	   (with-sections ,(cdr sections) ,@body)))
	(t
	 (let ((symbol-of-the-day (gensym)))
	   `(osicat:with-temporary-file (,symbol-of-the-day
					 :element-type '(unsigned-byte 8))
	     ,(create-stream-section (car sections) symbol-of-the-day)
	     (with-sections ,(cdr sections) ,@body))))))

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
