;;; Code generation-related routines for m68k-assembler.
;;;
;;; Julian Squires/2005

(in-package :m68k-assembler)

;;;; OPERAND CONSTRAINTS

(defparameter *all-ea-modes*
  '(immediate pc-index pc-displacement absolute-long absolute-short
    postincrement-indirect predecrement-indirect displacement-indirect
    indexed-indirect vanilla-indirect address-register data-register))

(defparameter *constraints-modes-table*
  `((all-ea-modes ,*all-ea-modes*)
    (alterable-modes ,(set-difference *all-ea-modes*
     '(immediate pc-index pc-displacement)))
    (data-alterable-modes ,(set-difference *all-ea-modes*
     '(immediate pc-index pc-displacement address-register)))
    (memory-alterable-modes ,(set-difference *all-ea-modes*
     '(immediate pc-index pc-displacement address-register data-register)))
    (data-addressing-modes ,(set-difference *all-ea-modes*
					    '(address-register)))
    (control-addressing-modes ,(set-difference *all-ea-modes*
     '(address-register data-register immediate predecrement-indirect
       postincrement-indirect)))
    (movem-pre-modes (vanilla-indirect predecrement-indirect
		      displacement-indirect indexed-indirect absolute-short
		      absolute-long))
    (movem-post-modes (vanilla-indirect postincrement-indirect
		       displacement-indirect indexed-indirect absolute-short
		       absolute-long pc-displacement pc-index))
    (absolute (absolute-short absolute-long))))

; memory-alterable-modes -> indirect-modes + absolute
; data-alterable-modes -> memory-alterable-modes + data-register
; alterable-modes -> data-alterable-modes + address-register

; data-addressing-modes -> all-ea-modes - address-register

; control-addressing-modes -> all-ea-modes - (registers + immediate +
;                                          postincrement-indirect +
;                                          predecrement-indirect)


;; transforms...
;;    displacement-indirect w/o expression => vanilla-indirect
;;    register dX => data-register
;;    register aX => address-register
;;    indirect w/pc => pc-indexed or pc-displacement

(defun refine-type (operand)
  (cond 
    ;; XXX: should try to evaluate expression if it exists.
    ((and (eql (car operand) 'displacement-indirect)
	  (zerop (indirect-displacement operand))
	  (address-register-p (indirect-base-register operand)))
     'vanilla-indirect)
    ((eql (car operand) 'register)
     (case (char (caadr operand) 0)
       ((#\d #\D) 'data-register)
       ((#\a #\A) 'address-register)
       (t 'register)))
    ;; XXX: not sure if this is reasonable behavior, but it seems ok
    ;; to me.
    ((eql (car operand) 'absolute)
     (if (absolute-definitely-needs-long-p operand)
	 'absolute-long
	 'absolute-short))
    (t (car operand))))

(defun operand-type-matches-constraint-type-p (operand constraint)
  (let ((refined-op (refine-type operand)))
    (aif (find (car constraint) *constraints-modes-table* :key #'car)
	 (member refined-op (second it))
	 (eql refined-op (car constraint)))))

(defun satisfies-operand-constraints-p (operands constraints)
  (when (equal (length operands) (length constraints))
    (do ((op-> operands (cdr op->))
	 (co-> constraints (cdr co->)))
	((and (null op->) (null co->)) t)
      (unless (operand-type-matches-constraint-type-p (car op->)
						      (car co->))
	(return nil))
      (dolist (v-constraint (cdar co->))
	(format t "~&v-constraint: ~A" v-constraint)))))

(defun satisfies-modifier-constraints-p (modifier mod-list)
  (cond ((null mod-list) (null modifier))
	((null modifier) t) ;; for the moment
	(t (member modifier mod-list))))


(defun find-matching-entry (opcode operands modifier)
  (dolist (entry (cdr (find opcode *asm-opcode-table* :key #'car
			    :test #'string-equal)))
    (cond ((stringp entry)		; redirect.
	   (format t "~&redirecting: ~A" entry)
	   (awhen (find-matching-entry entry operands modifier) it))
	  ((consp entry)
	   (when (and (satisfies-modifier-constraints-p modifier
							(caar entry))
		      (satisfies-operand-constraints-p operands
						       (cdar entry)))
	     (return entry)))
	  (t (error "Bad entry in opcode table: ~A: ~A" opcode entry)))))


;;;; CODE GENERATION HELPERS

;;; Note that basically anything here wants to work with
;;; pre-simplified parse trees.

(defun register-idx (operand &key both-sets)
  "If operand is a register or contains in some way a single register,
returns only the numeric index of that register, and the number of
bits that takes up, as a second value.  Otherwise, an error is
signalled."
  (case (car operand)
    (register)
    (t (setf operand (indirect-base-register operand))))

  (let ((idx (position (caadr operand) *asm-register-table*
		       :test #'string-equal :key #'car)))
    (assert (<= 0 idx 15))
    (if both-sets
	(values idx 4)
	(values (logand idx 7) 3))))

(defun register-modifier (operand)
  (assert (eql (car operand) 'register))
  (cadadr operand))

;;; XXX: naive
(defun data-register-p (operand)
  (char-equal (char (caadr operand) 0) #\D))
(defun address-register-p (operand)
  (char-equal (char (caadr operand) 0) #\A))
(defun pc-register-p (operand)
  (string-equal (caadr operand) "PC"))

(defun indirect-base-register (operand)
  (find 'register (cdr operand) :key #'carat))
(defun indirect-index-register (operand)
  (assert (eql (car operand) 'indexed-indirect))
  (find 'register (cdr operand) :from-end t :key #'carat))
(defun indirect-displacement (operand)
  (cond ((integerp (cadr operand)) (cadr operand))
	((not (eql (caadr operand) 'expression)) 0)
	(t (resolve-expression (cadr operand)))))

(defun bit-vector->int (vector)
  "Converts a bit-vector to an integer, assuming that array indices
correspond to bit places in the integer.  (For example, index 0 in
VECTOR corresponds to the least-significant bit of the return value.)"
  (do ((value 0 (+ (ash value 1) (aref vector i)))
       (i (1- (length vector)) (1- i)))
      ((< i 0) value)))

;;; XXX probably totally fucked in this modern world of Unicode.
(defun string->int (string)
  "Converts a string to an integer, assuming that character elements
correspond to bytes, and that they are limited in range from 0 to
255."
  (do ((value 0 (+ (ash value 8) (char-code (char string i))))
       (i 0 (1+ i)))
      ((>= i (length string)) value)))

(defun carat (x) (if (consp x) (car x) x))


(defun register-mask-list (operand &key flipped-p)
  (let ((bitmask (make-array '(16) :element-type 'bit :initial-element 0)))
    ;; iterate over operands
    (dolist (r (if (eql (car operand) 'register)
		   (list operand)
		   (cdr operand)))
      (if (eql (car r) 'register)
	(setf (aref bitmask (register-idx r :both-sets t)) 1)
	(warn "register-mask-list: ignoring ~A" r)))
    (values (bit-vector->int (if flipped-p (nreverse bitmask) bitmask))
	    16)))

(defun effective-address-mode (operand &key (flipped-p nil))
  "Calculates the classic six-bit effective address mode and register
values.  If FLIPPED-P, returns the mode and register values swapped."
  (flet ((combine (m r)
	   (logior (if flipped-p (ash r 3) r) (if flipped-p m (ash m 3)))))
    (values
     (ecase (car operand)
       (register
	(cond ((data-register-p operand) (combine 0 (register-idx operand)))
	      ((address-register-p operand) 
	       (combine #b001 (register-idx operand)))
	      ;; XXX I'm a bit iffy on this one, but so the book says...
	      ((string-equal (caadr operand) "SR") (combine #b111 #b100))))
       (displacement-indirect
	(let ((base (indirect-base-register operand)))
	  (cond ((pc-register-p base) (combine #b111 #b010))
		((eql (refine-type operand) 'vanilla-indirect)
		 (combine #b010 (register-idx base)))
		(t (combine #b101 (register-idx base))))))
       (indexed-indirect
	(let ((base (indirect-base-register operand)))
	  (cond ((pc-register-p base) (combine #b111 #b011))
		(t (combine #b110 (register-idx base))))))
       (absolute
	(ecase (refine-type operand)
	  (absolute-short (combine #b111 #b000))
	  (absolute-long (combine #b111 #b001))))
       (immediate (combine #b111 #b100))
       (postincrement-indirect
	(combine #b011 (register-idx (indirect-base-register operand))))
       (predecrement-indirect
	(combine #b100 (register-idx (indirect-base-register operand)))))
     6)))

(defun effective-address-extra (operand modifier)
  "Returns the extra data required for addressing OPERAND, along with
the length of that data."
  (ecase (car operand)
    (register (values 0 0))
    (displacement-indirect
     (if (eql (refine-type operand) 'vanilla-indirect)
	 (values 0 0)
	 (values (indirect-displacement operand) 16)))
    (indexed-indirect
     (let ((index (indirect-index-register operand)))
       (if (not (pc-register-p (indirect-base-register operand)))
	   ;; rrrr l000 dddd dddd
	   (values (logior (ash (register-idx index :both-sets t) 12)
			   (if (aand (register-modifier index)
				     (eql it 'long))
			       (ash #b1 11) 0)
			   (logand (indirect-displacement operand) #xff))
		   16)
	   (error "Not sure how to encode this!")))) ;XXX
    (absolute
      (ecase (refine-type operand)
	(absolute-short (values (absolute-value operand) 16))
	(absolute-long (values (absolute-value operand) 32))))
    ;; XXX test extent of value
    (immediate (values (immediate-value operand)
		       (if (eql modifier 'long) 32 16)))
    (postincrement-indirect (values 0 0))
    (predecrement-indirect (values 0 0))))

;; XXX this name sucks (too much like ABS)
(defun absolute-value (operand)
  ;; XXX deal with short and long
  (values (resolve-expression (second operand)) 16))

;;; XXX defaults to nil.
(defun absolute-definitely-needs-long-p (operand)
  (let ((v (resolve-expression (second operand))))
    (and (integerp v) (or (< v -32768)
			  (> v 65535)))))

(defun immediate-value (operand &optional modifier)
  "Returns a certain number of bits from the immediate value of
OPERAND, based on MODIFIER, if specified."
  (values (resolve-expression (second operand))
	  (case modifier (byte 8) (word 16) (long 32) (t '?))))

(defun modifier-bits (modifier)
  (values (ecase modifier (byte #b00) (word #b01) (long #b10)) 2))

(defun modifier-bits-for-move (modifier)
  (values (ecase modifier (byte #b01) (word #b11) (long #b10)) 2))


(defun branch-displacement-bits (operand modifier &key db-p)
  "Returns displacement of OPERAND relative to *PROGRAM-COUNTER*.  If
:DB-P is T, calculate displacement as per the DBcc opcodes (always
16-bit).  Otherwise, calculate displacement as per Bcc opcodes, where
displacement is either 8 bits or 16 padded to 24."
  (let ((value (absolute-value operand)))
    ;; Note PC+2 -- this is due to the way the m68k fetches
    ;; instructions.
    (values (or (and (integerp value)
		     (- value (+ *program-counter* 2)))
		value)
	    (cond (db-p 16)
		  ((eql modifier 'word) 24) ;XXX should zero top 8 bits.
		  (t 8)))))


;;;; TEMPLATE EVALUATOR


(defun make-codegen-sublis (operands modifier)
  (let ((sub (list (cons 'modifier modifier))))
    ;; Adjust as necessary for the number of operands possible.
    (do ((a-> operands (cdr a->))
	 (b-> '(first-operand second-operand third-operand) (cdr b->)))
	((or (null a->) (null b->)) sub)
      (push (cons (car b->) (car a->)) sub))))

(defun fill-codegen-template (list)
  (do* ((item-> list (cdr item->))
	(length (caar item->) (caar item->))
	(formula (cadar item->) (cadar item->))
	(done-p t))
       ((null item->) done-p)
    (when (consp formula)
      (multiple-value-bind (val val-len) (apply (car formula) (cdr formula))
	(unless (integerp length) (setf (caar item->) val-len))
	(when (integerp val-len) (assert (= (caar item->) val-len)))
	(if (integerp val)
	    (setf (cadar item->) val)
	    (setf done-p nil))))))

;;; first do the sublis, then walk through each item doing the
;;; evaluation when we can.  if everything's reduced to an atom,
;;; compile it and return it.  otherwise, return the half-completed
;;; list.

;; if the first is an integer, add it to the result-length.  if the
;; second is an atom, or it into the result.  if the second is a list,
;; evaluate it with first-operand, second-operand, and modifier bound.
(defun generate-code (template operands modifier)
  (let ((result 0)
	(result-len 0)
	(list (sublis (make-codegen-sublis operands modifier) template)))

    (cond ((fill-codegen-template list)
	   (dolist (item list)
	     (destructuring-bind (len val) item
	       (setf result (logior (ldb (byte len 0) val)
				    (ash result len)))
	       (incf result-len len)))
	   (values result result-len))
	  (t (dolist (item list) (incf result-len (first item)))
	     (values list result-len)))))

;;;; EOF codegen.lisp
