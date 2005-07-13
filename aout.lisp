;;;
;;; a.out object file support for m68k-assembler.
;;;
;;; Julian Squires / 2005

(in-package :m68k-assembler)


(defun finalize-object-file (name section-lengths)
  "Finalize object file (collect sections, write symbol table,
patch header)."
  (with-open-file (output-stream name :direction :output
				 :element-type 'unsigned-byte
				 :if-exists :new-version
				 :if-does-not-exist :create)
    (write-big-endian-data output-stream #x53544F26 32) ; Magic
    ;; Text, Data, BSS
    (mapcar (lambda (x)
	      (write-big-endian-data output-stream (cdr x) 32))
	    section-lengths)
    ;; symbol table
    (write-big-endian-data output-stream
			   (hash-table-count *symbol-table*)
			   32)
    ;; entry point
    (write-big-endian-data output-stream 0 32)
    ;; text reloc size
    (write-big-endian-data output-stream
			   (hash-table-count *relocation-table*)
			   32)
    ;; data reloc size
    (write-big-endian-data output-stream
			   0
			   32)

    (copy-stream-contents (cdr (assoc 'text *object-streams*))
			  output-stream)
    (copy-stream-contents (cdr (assoc 'data *object-streams*))
			  output-stream)
    ;; revise symbol table/relocs to use numeric indices
    (let* ((symbols (serialize-symbol-table)))
      ;; output relocations
      (maphash (lambda (k v)
		 (declare (ignore k))
		 ;; address
		 (write-big-endian-data output-stream
					(relocation-address v)
					32)
		 ;; index|24 pc-rel-p length|2 extern-p spare|4
		 (write-big-endian-data
		  output-stream
		  (logior (ash (relocation-index-bits v symbols) 8)
			  (ash (if (relocation-pc-relative-p v) 1 0) 7)
			  (ash (ceiling (log (relocation-size v) 2)) 5)
			  (ash (if (relocation-extern-p v) 1 0) 4))
		  32))
	       *relocation-table*)
      ;; output symbol table
      (do ((i 0 (1+ i))
	   (sym))
	  ((>= i (length symbols)))
	(setf sym (aref symbols i))
	(write-big-endian-data output-stream i 32)
	(write-big-endian-data
	 output-stream
	 (logior (ash (position (asm-symbol-type sym)
				'(text data bss absolute extern))
		      25)
		 (ash (if (asm-symbol-global-p sym) 1 0) 24)
		 (aout-munge-debug-info (asm-symbol-debug-info sym)))
	 32)
	(write-big-endian-data output-stream (asm-symbol-value sym) 32))
      ;; output string table
      (dotimes (i (length symbols))
	(write-string (asm-symbol-name (aref symbols i)) output-stream)
	(write-byte 0 output-stream)))))

;;; XXX we could do something much more sophisticated, such as having
;;; debug info serve as an index into a debugging info table later in
;;; the file, but who cares for now?
(defun aout-munge-debug-info (debug-info)
  (lexer-state-line debug-info))

(defun relocation-index-bits (v symbols)
  (if (relocation-extern-p v)
      (position (relocation-symbol v) symbols :test #'string-equal)
      (position (relocation-segment v) '(text data bss))))

(defun serialize-symbol-table ()
  (let ((table (make-array (list 0))))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (when (asm-symbol-global-p v)
		 (vector-push-extend v table)))
	     *symbol-table*)
    table))

(defun serialize-reloc-table ()
  (let ((table (make-array (list (hash-table-count *relocation-table*)))))
    (with-hash-table-iterator (next *relocation-table*)
      (dotimes (i (length table))
	(multiple-value-bind (more-p k v) (next)
	  (declare (ignore k))
	  (assert more-p)
	  (setf (aref table i) v))))
    table))