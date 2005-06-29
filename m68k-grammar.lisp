
;;; m68k asm grammar

(in-package :m68k-assembler)

(defparameter *m68k-asm-grammar*
  '(;; OPERAND
    line ((label)
	  (label operation operands)
	  (operation)
	  (operation operands))
    label ((symbol) (symbol colon))

    operands ((operand)
	      (operands comma operand))

    operation ((opcode)
	       (pseudo-op))

    ;; ADDRESSING MODES

    operand ((absolute)
	     (indirect)
	     (immediate)
	     (register-list))

    absolute ((expression))

    indirect ((displacement-indirect)
	      (postincrement-indirect)
	      (predecrement-indirect)
	      (indexed-indirect))

    displacement-indirect ((expression open register close)
			   (open register close))
    postincrement-indirect ((open register close +))
    predecrement-indirect ((- open register close))
    indexed-indirect ((expression open register comma register close)
		      (open register comma register close))

    immediate ((hash expression))

    ;; we use register-list also for register-direct, and sort out the
    ;; difference at a semantic level.
    register-list ((register)
		   (register - register)
		   (register-list / register-list))

    ;; EXPRESSIONS

    expression ((term)
		(expression adding-operator term))
    adding-operator ((+) (-) (<<) (>>) (^) (or) (&))

    term ((factor)
	  (term multiplying-operator factor))
    multiplying-operator ((*) (/))

    factor ((value)
	    (unary-operator value)
	    (open expression close))
    unary-operator ((~) (-))

    value ((constant)
	   (symbol))))
