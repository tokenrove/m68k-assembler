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
    (movem-pre-modes '(vanilla-indirect predecrement-indirect
		       displacement-indirect indexed-indirect absolute-short
		       absolute-long))
    (movem-post-modes '(vanilla-indirect postincrement-indirect
		       displacement-indirect indexed-indirect absolute-short
		       absolute-long pc-displacement pc-index))))

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
	  (not (eql (caadr operand) 'expression)))
     'vanilla-indirect)
    ((eql (car operand) 'register)
     (case (char (caadr operand) 0)
       ((#\d #\D) 'data-register)
       ((#\a #\A) 'address-register)
       (t 'register)))
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
  (cond ;((null modifier) (guess-modifier))
	((null mod-list) (null modifier))
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

(defun register-idx (operand &key both-sets)
  "If operand is a register or contains in some way a single register,
returns only the numeric index of that register, and the number of
bits that takes up, as a second value.  Otherwise, an error is
signalled."
  (assert (eql (car operand) 'register))
  (let ((idx (position (caadr operand) *asm-register-table*
		       :test #'string-equal :key #'car)))
    (assert (<= 0 idx 15))
    (if both-sets
	(values idx 4)
	(values (logand idx 7) 3))))

(defun indirect-base-register (operand))
(defun indirect-index-register (operand))

(defun register-mask-list (operand &key flipped-p)
  (values 0 16))

(defun effective-address-mode (operand &key (flipped-p nil))
  "Calculates the classic six-bit effective address mode and register
values.  If FLIPPED-P, returns the mode and register values swapped."
  ;; return value and number of bits.
  (values 0 6))

(defun effective-address-extra (operand)
  "Returns the extra data required for addressing OPERAND, along with
the length of that data."
  (values 0 ?))

(defun immediate-value (operand &optional modifier)
  "Returns a certain number of bits from the immediate value of
OPERAND, based on MODIFIER, if specified."
  (values 0 (case modifier (byte 8) (word 16) (long 32) (nil '?))))

(defun modifier-bits (modifier)
  (values (ecase modifier (byte #b00) (word #b01) (long #b10)) 2))

(defun branch-displacement-bits (operand modifier)
  ;; either 8 or 24 (top 8 bits set to zero)
  (values 8 (absolute-value operand)))

