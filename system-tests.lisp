;;; Test things externally.

(in-package :m68k-assembler-tests)

(defparameter *base-path* "/home/julian/projects/m68k-assembler/")

;; XXX this doesn't actually work, because the symbol table outputs
;; differ between compilers.
(deftest ymamoto.regress.1
    (let ((orig-filename (merge-pathnames "tests/ymamoto.o.test" *base-path*))
	  (our-filename (merge-pathnames "tests/ymamoto.o" *base-path*)))
      (m68k-assembler:assemble (merge-pathnames "tests/ymamoto.s" *base-path*)
			       :object-name our-filename)
      (with-open-file (original orig-filename :direction :input
				:element-type 'unsigned-byte)
	(with-open-file (new our-filename :direction :input
			     :element-type 'unsigned-byte)
	  (unless (= (file-length original) (file-length new))
	    (error "Lengths differ in output."))
	  (loop for a = (read-byte original nil nil)
		and b = (read-byte new nil nil)
		when (not (equal a b))
		do (error "a (~S) and b (~S) differ." a b)
		while (and a b))
	  (unless (and (equal (read-byte new nil nil) nil)
		       (equal (read-byte original nil nil) nil))
	    (error "Outputs not the same at EOF"))))
      t)
  t)
