;;;
;;; Mostly-a.out object file support for m68k-assembler.
;;;
;;; Julian Squires / 2005

(in-package :m68k-assembler)


(defun finalize-object-file (name section-lengths)
  "Finalize a.out object file (collect sections, write symbol table,
patch header)."
  (with-open-file (output-stream name :direction :output
				 :element-type 'unsigned-byte
				 :if-exists :new-version
				 :if-does-not-exist :create)
    ;; revise symbol table/relocs to use numeric indices
    (let* ((symbols (serialize-symbol-table)))
      (write-big-endian-data output-stream #x53544F26 32) ; Magic
      ;; section lengths.
      (dolist (x '(text data bss))
	(write-big-endian-data output-stream
			       (cdr (assoc x section-lengths))
			       32))
      (dolist (x '(text data bss))
	(format t "~&~A: ~A bytes." x (cdr (assoc x section-lengths))))
      ;; symbol table
      (write-big-endian-data output-stream (length symbols) 32)
      ;; entry point
      (write-big-endian-data output-stream 0 32)
      ;; reloc sizes
      (dolist (x '(text data))
	(write-big-endian-data output-stream
			       (hash-table-count
				(section-relocations
				 (cdr (assoc x *sections*))))
			       32))

      ;; actual contents of sections.
      (dolist (x '(text data))
	(copy-stream-contents (section-object-stream (cdr (assoc x *sections*)))
			      output-stream))

      ;; output relocations
      (dolist (x '(text data))
	(output-reloc-table output-stream
			    (section-relocations (cdr (assoc x *sections*)))
			    symbols))

      (output-symbol-table output-stream symbols)
      (output-string-table output-stream symbols))))

(defun output-reloc-table (output-stream relocs symbols)
  (maphash (lambda (k v)
	     (declare (ignore k))
	     ;; address
	     (write-big-endian-data output-stream
				    (relocation-address v)
				    32)
	     ;; index|24 pc-rel-p length|2 extern-p spare|4
	     ;; XXX should use dpb to avoid overflows
	     (write-big-endian-data
	      output-stream
	      (logior (ash (relocation-index-bits v symbols) 8)
		      (ash (if (relocation-pc-relative-p v) 1 0) 7)
		      (ash (if (relocation-extern-p v) 1 0) 6)
		      (ldb (byte 6 0) (relocation-size v)))
	      32))
	   relocs))

(defun output-symbol-table (output-stream symbols)
  ;; output symbol table
  (do ((i 0 (1+ i))
       (sym))
      ((>= i (length symbols)))
    (setf sym (aref symbols i))
    (format t "~&~A ~A" (asm-symbol-name sym) (asm-symbol-type sym))
    (assert (and (asm-symbol-value sym)
		 (asm-symbol-type sym)))
    (write-big-endian-data output-stream i 32)
    (write-big-endian-data
     output-stream
     (logior (ash (section->bits (asm-symbol-type sym))
		  25)
	     (ash (if (asm-symbol-global-p sym) 1 0) 24)
	     (aout-munge-debug-info (asm-symbol-debug-info sym)))
     32)
    (write-big-endian-data output-stream (asm-symbol-value sym) 32)))

(defun section->bits (section)
  (position section '(text data bss absolute extern)))

;;; XXX needs to be fixed.
(defun output-string-table (output-stream symbols)
  (dotimes (i (length symbols))
    (let ((sym (asm-symbol-name (aref symbols i))))
      (write-sequence #+sb-unicode (sb-ext:string-to-octets sym)
		      #-sb-unicode (coerce (map 'list #'char-code sym)
					   '(vector unsigned-byte))
		      output-stream))
    (write-byte 0 output-stream)))

;;; XXX we could do something much more sophisticated, such as having
;;; debug info serve as an index into a debugging info table later in
;;; the file, but who cares for now?
(defun aout-munge-debug-info (debug-info)
  (lexer-state-line debug-info))

(defun relocation-index-bits (v symbols)
  (if (relocation-extern-p v)
      (position (relocation-symbol v) symbols :test #'string-equal
		:key #'asm-symbol-name)
      (position (relocation-segment v) '(text data bss))))


