;;; Abstract Syntax Tree-ish tools for m68k-assembler.
;;;
;;; Julian Squires / 2005
;;;

(in-package :m68k-assembler)

;;;; PARSE TREE MANIPULATORS

;;; XXX Not the most efficient way to get a list of operands, but I am
;;; getting tired.
(defun extract-operands-1 (parse-tree)
  (let ((list))
    (dolist (x parse-tree)
      (when (consp x)
	(when (eql (car x) 'operand)
	  (push (cadr x) list))
	(when (eql (car x) 'operands)
	  (setf list (append (extract-operands-1 x) list)))))
    list))

(defun extract-operands (parse-tree)
  (reverse (extract-operands-1 parse-tree)))


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
  (format t "~&interpolate registers from ~A to ~A"
	  (register-idx start :both-sets t)
	  (register-idx end :both-sets t))
  (loop for i from (register-idx start :both-sets t)
	to (register-idx end :both-sets t)
	collecting (list (car (aref *asm-register-table* i)))))

;;; XXX could use some work.
(defun simplify-register-list (tree)
  (assert (eql (car tree) 'register-list))
  (cond ((= (length tree) 2) (second tree))
	((= (length tree) 4)
	 (cond ((eql (car (third tree)) '/)
		(list (simplify-register-list (second tree))
		      (simplify-register-list (fourth tree))))
	       ((eql (car (third tree)) '-)
		(interpolate-registers (second tree) (fourth tree)))
	       (t (error "Strange register list."))))
	(t (error "Strange parse tree."))))

;;;; Expression simplifiers.

;;; Because we know expressions only contain certain kinds of
;;; elements, we can easily reduce them down to "almost" lisp
;;; expressions.
(defun simplify-operator (tree)
  (assert (member (first tree) '(adding-operator multiplying-operator
				 unary-operator)))
  (second tree))

(defun simplify-factor (tree)
  (cond ((= (length tree) 2) (second (second tree)))
	((= (length tree) 3) (list (simplify-operator (second tree))
				   (second (third tree))))
	((= (length tree) 4) (list (simplify-expression (third tree))))
	(t (error "Strange parse tree."))))

(defun simplify-term (tree)
  (cond ((= (length tree) 2) (simplify-factor (second tree)))
	((= (length tree) 4)
	 (list (simplify-operator (third tree))
	       (simplify-term (second tree))
	       (simplify-factor (fourth tree))))
	(t (error "Strange parse tree."))))

(defun simplify-expression (tree)
  (cond ((= (length tree) 2) (simplify-term (second tree)))
	((= (length tree) 4)
	 (list (simplify-operator (third tree))
	       (simplify-expression (second tree))
	       (simplify-term (fourth tree))))
	(t (error "Strange parse tree."))))
