;;; Abstract Syntax Tree-ish tools for m68k-assembler.
;;;
;;; Julian Squires / 2005
;;;

(in-package :m68k-assembler)

;;;; PARSE TREE MANIPULATORS

;;; XXX Not the most efficient way to get a list of operands, but I am
;;; getting tired.
(defun extract-operands (parse-tree)
  (labels ((inner-fn (parse-tree)
	     (let ((list))
	       (dolist (x parse-tree)
		 (when (consp x)
		   (when (eql (car x) 'operand)
		     (push (cadr x) list))
		   (when (eql (car x) 'operands)
		     (setf list (nconc (inner-fn x) list)))))
	       list)))
    (reverse (inner-fn parse-tree))))


(defun simplify-operand (tree)
  (case (car tree)
    (immediate (simplify-immediate tree))
    (register-list (simplify-register-list tree))
    (absolute (simplify-absolute tree))
    (indirect (simplify-indirect tree))
    (t tree)))

;;;; Addressing mode simplifiers.

(defun simplify-immediate (tree)
  (assert (and (eql (car tree) 'immediate)
	       (eql (caadr tree) 'hash)))
  (list 'immediate (simplify-expression (third tree))))

(defun simplify-absolute (tree)
  (assert (eql (car tree) 'absolute))
  (list 'absolute (simplify-expression (second tree))))

(defun simplify-indirect (tree)
  (assert (eql (car tree) 'indirect))
  (case (caadr tree)
    (displacement-indirect
     (cond ((= (length (cadr tree)) 5)
	    (list (caadr tree) (simplify-expression (second (cadr tree)))
		  (fourth (cadr tree))))
	   ((= (length (cadr tree)) 4)
	    (list (caadr tree) (third (cadr tree))))
	   (t (error "Weird parse tree: ~A." tree))))
    (indexed-indirect
     (cond ((= (length (cadr tree)) 7)
	    (list (caadr tree) (simplify-expression (second (cadr tree)))
		  (fourth (cadr tree)) (sixth (cadr tree))))
	   ((= (length (cadr tree)) 6)
	    (list (caadr tree) (third (cadr tree)) (fifth (cadr tree))))
	   (t (error "Weird parse tree: ~A." tree))))
    (postincrement-indirect
     (list (caadr tree) (third (cadr tree))))
    (predecrement-indirect
     (list (caadr tree) (fourth (cadr tree))))
    (t (error "Weird indirect: ~A." tree))))

(defun interpolate-registers (start end)
  (let ((s (register-idx start :both-sets t))
	(e (register-idx end :both-sets t)))
    (when (> s e) (psetf s e e s))
    (loop for i from s to e
	  collecting `(register
		       ,(cons (car (aref *asm-register-table* i)) nil)))))

;;; XXX could use some work.
(defun simplify-register-list (tree)
  (labels ((simplify (tree)
	     (assert (eql (car tree) 'register-list))
	     (cond ((= (length tree) 2) (list 'register-list
					      (second tree)))
		   ((= (length tree) 4)
		    (cond ((eql (car (third tree)) '/)
			   (cons 'register-list
				 (append 
				  (cdr (simplify (second tree)))
				  (cdr (simplify (fourth tree))))))
			  ((eql (car (third tree)) '-)
			   (cons 'register-list
				 (interpolate-registers (second tree) 
							(fourth tree))))
			  (t (error "Strange register list."))))
		   (t (error "Strange parse tree.")))))
    ;; If this is a register list of a single register, return just
    ;; the register.
    (let ((v (simplify tree)))
      (if (= (length v) 2) (second v) v))))

;;;; Expression simplifiers.

;;; Because we know expressions only contain certain kinds of
;;; elements, we can easily reduce them down to "almost" lisp
;;; expressions.
(defun simplify-operator (tree)
  (assert (member (first tree) '(adding-operator multiplying-operator
				 bitwise-operator unary-operator)))
  (car (second tree)))

(defun simplify-value (tree)
  (second tree))


;; XXX change all this stuff to something data driven:
;;(factor (nil (simplify-value))
;;	(nil (simplify-operator) (simplify-value))
;;	(nil nil (simplify-expression) nil))

(defun simplify-factor (tree)
  (cond ((= (length tree) 2)
	 (let ((v (simplify-value (second tree))))
	   (if (eql (car v) 'constant) (second v) v)))
	((= (length tree) 3) (list (simplify-operator (second tree))
				   (simplify-value (third tree))))
	((= (length tree) 4) (simplify-expression (third tree)))
	(t (error "Strange parse tree."))))

(defun simplify-term (tree)
  (cond ((= (length tree) 2) (simplify-factor (second tree)))
	((= (length tree) 4)
	 (list (simplify-operator (third tree))
	       (simplify-term (second tree))
	       (simplify-factor (fourth tree))))
	(t (error "Strange parse tree."))))

(defun simplify-term2 (tree)
  (cond ((= (length tree) 2) (simplify-term (second tree)))
	((= (length tree) 4)
	 (list (simplify-operator (third tree))
	       (simplify-term2 (second tree))
	       (simplify-term (fourth tree))))
	(t (error "Strange parse tree."))))

(defun simplify-expression (tree)
  (cond ((= (length tree) 2) (simplify-term2 (second tree)))
	((= (length tree) 4)
	 (list (simplify-operator (third tree))
	       (simplify-expression (second tree))
	       (simplify-term2 (fourth tree))))
	(t (error "Strange parse tree."))))

;;;; UNCLUTTER/STRIP-POSITION

(defun unclutter-line (tree)
  "Takes a parse tree, strips the individual token position
information, and returns two values -- the uncluttered parse tree, and
a single representative item of position information."
  (let ((position nil))
    (labels ((strip (branch)
	       (cond ((atom branch))
		     ((terminal-p (car branch))
		      (assert (is-position-info-p (third branch)))
		      ;; XXX would be nice to improve this "merge" to be
		      ;; a bit more heuristic (watch for merging pos from
		      ;; separate files, for example... !)
		      (when (null position) (setf position (third branch)))
		      (setf (cddr branch) nil))
		     (t
		      (dolist (leaf branch) (strip leaf))))))
      (strip tree)
      (values tree position))))


;;;; EOF ast.lisp
