
(in-package :m68k-assembler)

; memory-alterable-modes -> indirect-modes + absolute
; data-alterable-modes -> memory-alterable-modes + data-register
; alterable-modes -> data-alterable-modes + address-register

; data-addressing-modes -> all-modes - address-register

; control-addressing-modes -> all-modes - (registers + immediate +
;                                          postincrement-indirect +
;                                          predecrement-indirect)


;;; We might want to put these somewhere more machine-related.
(defparameter *asm-opcode-table*
  ;; first field is the opcode name, sans modifiers.  remaining fields
  ;; are either pairs of operand constraints and code generation
  ;; masks, or functions.  these are matched or called in sequence,
  ;; until one can be found which matches.
  ;;
  ;; Code generation masks are lists in the form:
  ;;  (size code) where size indicates the number of bits this part
  ;;  should take up, and code is the code to evaluate when generating
  ;;  this part of the expression.
  ;; The list starts from the most significant bit of the instruction
  ;; word and moves down.  SIZE fields of ? indicate a variable size,
  ;; where the code in question will return a suitable size value as a
  ;; second value.
  `(,@(mapcar
       (lambda (x)
	 `(,x
	   (((byte) (data-register) (data-register)) .
	    ((4 ,(if (string= x "ABCD") #b1101 #b1001))
	     (3 (register-idx second-operand)) (6 #b100000)
	     (3 (register-idx first-operand))))
	   (((byte) (predecrement-indirect) (predecrement-indirect))
	    ((4 ,(if (string= x "ABCD") #b1101 #b1001))
	     (3 (register-idx second-operand)) (6 #b100001)
	     (3 (register-idx first-operand))))))
       '("ABCD" "SBCD"))

    ,@(mapcan
       (lambda (x)
	 `((,x
	    (((byte word long) (all-modes) (data-register))
	     ((4 ,(if (string= x "ADD") #b1101 #b1001))
	      (3 (register-idx second-operand)) (1 #b1)
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode first-operand))
	      (? (effective-address-extra first-operand))))
	    (((byte word long) (data-register) (alterable-memory-modes))
	     ((4 ,(if (string= x "ADD") #b1101 #b1001))
	      (3 (register-idx first-operand)) (1 #b0)
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (effective-address-extra second-operand))))
	    ,(concatenate 'string x "A")
	    ,(concatenate 'string x "I")
	    ,(concatenate 'string x "Q")
	    ,(concatenate 'string x "X"))
	   (,(concatenate 'string x "A")
	    (((word long) (all-modes) (address-register))
	     ((4 ,(if (string= x "ADD") #b1101 #b1001))
	      (3 (register-idx second-operand))
	      (1 (if (eql modifier 'word) 0 1))
	      (2 #b11) (6 (effective-address-mode first-operand))
	      (? (effective-address-extra first-operand)))))
	   (,(concatenate 'string x "I")
	    (((byte word long) (immediate) (data-alterable-modes))
	     ((8 ,(if (string= x "ADD") #b00000110 #b00000100))
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (immediate-value first-operand modifier))
	      (? (effective-address-extra second-operand)))))
	   (,(concatenate 'string x "Q")
	    (((byte word long) (immediate (<= 1 value 8)) (alterable-modes))
	     ((4 #b0101) (3 (immediate-value first-operand))
	      (1 ,(if (string= x "ADD") 0 1))
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (effective-address-extra second-operand)))))
	   (,(concatenate 'string x "X")
	    (((byte word long) (data-register) (data-register))
	     ((4 ,(if (string= x "ADD") #b1101 #b1001))
	      (3 (register-idx second-operand)) (1 #b1)
	      (2 (modifier-bits modifier))
	      (3 #b000) (3 (register-idx first-operand))))
	    (((byte word long) (predecrement-indirect) (predecrement-indirect))
	     ((4 #b1101) (3 (register-idx second-operand)) (1 #b1)
	      (2 (modifier-bits modifier))
	      (3 #b001) (3 (register-idx first-operand)))))))
       '("ADD" "SUB"))

    ,@(mapcan
       (lambda (x)
	 `((,x
	    ,@(unless (string= x "EOR")
	      `(((byte word long) (data-addressing-modes) (data-register))
		((4 ,(cond ((string= x "AND") #b1100)
			   ((string= x "OR") #b1000)))
		 (3 (register-idx second-operand)) (1 #b1)
		 (2 (modifier-bits modifier))
		 (6 (effective-address-mode first-operand))
		 (? (effective-address-extra first-operand)))))
	    (((byte word long) (data-register) (alterable-memory-modes))
	     ((4 ,(cond ((string= x "AND") #b1100)
			((string= x "EOR") #b1011)
			((string= x "OR") #b1000)))
	      (3 (register-idx second-operand)) (1 #b0)
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode first-operand))
	      (? (effective-address-extra first-operand))))
	    ,(concatenate 'string x "I"))
	   (,(concatenate 'string x "I")
	    (((byte word long) (immediate) (data-alterable-modes))
	     ((4 #b0000)
	      (4 ,(cond ((string= x "AND") #b0010)
			((string= x "EOR") #b1010)
			((string= x "OR") #b0000)))
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (immediate-value first-operand modifier))
	      (? (effective-address-extra second-operand))))
	    (((byte) (immediate) (register (string-equal value "CCR")))
	     ((4 #b0000)
	      (4 ,(cond ((string= x "AND") #b0010)
			((string= x "EOR") #b1010)
			((string= x "OR") #b0000)))
	      (16 #b0011110000000000)
	      (8 (immediate-value first-operand 'byte))))
	    (((word) (immediate) (register (string-equal value "SR")))
	     ((4 #b0000)
	      (4 ,(cond ((string= x "AND") #b0010)
			((string= x "EOR") #b1010)
			((string= x "OR") #b0000)))
	      (8 #b01111100)
	      (16 (immediate-value first-operand 'word)))))))
       '("AND" "EOR" "OR"))

    ,@(mapcar
       (lambda (x)
	 `(,x
	   (((byte word long) (data-register) (data-register))
	    ((4 #b1110) (3 (register-idx first-operand))
	     (1 ,(if (string= x "L" :start1 (1- (length x))) 1 0))
	     (2 (modifier-bits modifier))
	     (3 ,(cond ((string= x "LS" :end1 2) #b001)
		       ((string= x "AS" :end1 2) #b000)
		       ((string= x "ROX" :end1 3) #b010)
		       (t #b011)))
	     (3 (register-idx second-operand))))
	   (((byte word long) (immediate) (data-register))
	    ((4 #b1110) (3 (immediate-value first-operand))
	     (1 ,(if (string= x "L" :start1 (1- (length x))) 1 0))
	     (2 (modifier-bits modifier))
	     (3 ,(cond ((string= x "LS" :end1 2) #b101)
		       ((string= x "AS" :end1 2) #b100)
		       ((string= x "ROX" :end1 3) #b110)
		       (t #b111)))
	     (3 (register-idx second-operand))))
	   (((byte word long) (alterable-memory-modes))
	    ((5 #b11100)
	     (2 ,(cond ((string= x "LS" :end1 2) #b01)
		       ((string= x "AS" :end1 2) #b00)
		       ((string= x "ROX" :end1 3) #b10)
		       (t #b11)))
	     (1 ,(if (string= x "L" :start1 (1- (length x))) 1 0))
	     (2 #b11) (6 (effective-address-mode first-operand))
	     (? (effective-address-extra first-operand))))))
       '("ASL" "ASR" "LSL" "LSR" "ROL" "ROR" "ROXL" "ROXR"))

    ,@(mapcar
       (lambda (x)
	 `(,(car x)
	   (((byte word) (absolute))
	    ((4 #b0110) (4 ,(cdr x))
	     (? (branch-displacement-bits first-operand modifier))))))
       '(("BCC" . #b0100) ("BCS" . #b0101) ("BEQ" . #b0111) ("BGE" . #b1100)
	 ("BGT" . #b1110) ("BHI" . #b0010) ("BLE" . #b1111) ("BLS" . #b0011)
	 ("BLT" . #b1101) ("BMI" . #b1011) ("BNE" . #b0110) ("BPL" . #b1010)
	 ("BVC" . #b1000) ("BVS" . #b1001) ("BRA" . #b0000) ("BSR" . #b0001)))

    ,@(mapcar
       (lambda (x)
	 `(,(car x)
	   (((byte long) (data-register) (data-alterable-modes))
	    ((4 #b0000) (3 (register-idx first-operand))
	     (1 #b1) (2 ,(cdr x))
	     (6 (effective-address-mode second-operand))
	     (? (effective-address-extra second-operand))))
	   (((byte long) (immediate) (data-alterable-modes))
	    ((8 #b00001000) (2 ,(cdr x))
	     (6 (effective-address-mode second-operand))
	     (8 #b00000000) (8 (immediate-value immediate 'byte))
	     (? (effective-address-extra second-operand))))))
       '(("BCHG" . #b01) ("BCLR" . #b10) ("BSET" . #b11) ("BTST" . #b00)))

    ("CHK"
     (((word) (data-addressing-modes) (data-register))
      ((4 #b0100) (3 (register-idx second-operand)) (3 #b110)
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand)))))
    
    ,@(mapcar
       (lambda (x)
	 `(,x
	   (((byte word long) (data-alterable-modes))
	    ((5 #b01000)
	     (3 ,(cond ((string= x "NOT") #b110)
		       ((string= x "NEG") #b100)
		       ((string= x "NEGX") #b000)
		       ((string= x "CLR") #b010)))
	     (2 (modifier-bits modifier))
	     (6 (effective-address-modes first-operand))
	     (? (effective-address-extra first-operand))))))
       '("CLR" "NEG" "NEGX" "NOT"))

    ("CMP"
     (((byte word long) (all-modes) (data-register))
      ((4 #b1011) (3 (register-idx second-operand)) (1 #b0)
       (2 (modifier-bits modifier))
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand))))
     "CMPA" "CMPI" "CMPM")
    ("CMPA"
     (((word long) (all-modes) (address-register))
      ((4 #b1011) (3 (register-idx second-operand))
       (3 (if (eql modifier 'word) #b011 #b111))
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand)))))
    ("CMPI"
     (((byte word long) (immediate) (data-alterable-modes))
      ((8 #b00001100) (2 (modifier-bits modifier))
       (6 (effective-address-mode second-operand))
       (? (immediate-value first-operand modifier))
       (? (effective-address-extra second-operand)))))
    ("CMPM"
     (((byte word long) (postincrement-indirect) (postincrement-indirect))
      ((4 #b1011) (3 (register-idx second-operand))
       (1 #b1) (2 (modifier-bits modifier)) (3 #b001)
       (3 (register-idx first-operand)))))

    ,@(mapcar
       (lambda (x)
	 `(,(car x)
	   (((word) (data-register) (absolute))
	    ((4 #b0101) (4 ,(cdr x)) (5 #b11001)
	     (3 (register-idx first-operand))
	     (? (branch-displacement-bits first-operand modifier))))))
       '(("DBCC" . #b0100) ("DBCS" . #b0101) ("DBEQ" . #b0111)
	 ("DBGE" . #b1100) ("DBGT" . #b1110) ("DBHI" . #b0010)
	 ("DBLE" . #b1111) ("DBLS" . #b0011) ("DBLT" . #b1101)
	 ("DBMI" . #b1011) ("DBNE" . #b0110) ("DBPL" . #b1010)
	 ("DBVC" . #b1000) ("DBVS" . #b1001) ("DBT" . #b0000)
	 ("DBRA" . #b0000) ("DBF" . #b0001)))

    ,@(mapcar
       (lambda (x)
	 `(,x
	   (((word) (data-addressing-modes) (data-register))
	    ((4 ,(if (or (string= x "DIVS") (string= x "DIVU"))
		     #b1000 #b1100))
	     (3 (register-idx second-operand))
	     (3 ,(if (or (string= x "DIVS") (string= x "MULS"))
		     #b111 #b011))
	     (6 (effective-address-mode first-operand))
	     (? (effective-address-extra first-operand))))))
       '("DIVS" "DIVU" "MULS" "MULU"))

    ("EXG")

    ("EXT")

    ("ILLEGAL" (nil ((16 #b0100101011111100))))

    ("JMP"
     ((nil (control-addressing-modes))
      ((10 #b0100111011) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand)))))
    ("JSR"
     ((nil (control-addressing-modes))
      ((10 #b0100111010) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand)))))

    ("LEA")

    ("LINK")

    ("MOVE"
     ;; ea, ea
     ;; from CCR, to CCR
     ;; from SR, to SR
     ;; USP
     "MOVEA" "MOVEQ")
    ("MOVEA")
    ("MOVEC")
    ("MOVEM")
    ("MOVEP")
    ("MOVEQ")
    ("MOVES")

    ("NBCD"
     (((byte) (data-alterable-modes))
      ((10 #b0100100000) (6 (effective-address-modes first-operand))
       (? (effective-address-extra first-operand)))))

    ("RESET" (nil ((16 #b0100111001110000))))
    ("NOP"   (nil ((16 #b0100111001110001))))
    ("STOP"  (nil ((16 #b0100111001110010))))
    ("RTE"   (nil ((16 #b0100111001110011))))
    ("RTD"   (nil ((16 #b0100111001110100))))
    ("RTS"   (nil ((16 #b0100111001110101))))
    ("TRAPV" (nil ((16 #b0100111001110110))))
    ("RTR"   (nil ((16 #b0100111001110111))))

    ("PEA")

    ,@(mapcar
       (lambda (x)
	 `(,(car x)
	   (((byte) (data-alterable-modes))
	    ((4 #b0101) (4 ,(cdr x)) (2 #b11)
	     (6 (effective-mode-mode first-operand))
	     (? (effective-mode-extra first-operand))))))
       '(("SCC" . #b0100) ("SCS" . #b0101) ("SEQ" . #b0111) ("SGE" . #b1100)
	 ("SGT" . #b1110) ("SHI" . #b0010) ("SLE" . #b1111) ("SLS" . #b0011)
	 ("SLT" . #b1101) ("SMI" . #b1011) ("SNE" . #b0110) ("SPL" . #b1010)
	 ("SVC" . #b1000) ("SVS" . #b1001) ("ST" . #b0000) ("SF" . #b0001)))

    ("SWAP")

    ("TAS")

    ("TRAP")

    ("TST")

    ("UNLK")))

(defparameter *asm-pseudo-op-table*
  #(("SECTION" code)
    ("INCLUDE" code)
    ("MACRO")
    ("ENDM")
    ("REPT")
    ("ENDR")
    ("DC")
    ("DS")))

;; The order of elements in this table are significant.
(defparameter *asm-register-table*
  #(("d0") ("d1") ("d2") ("d3") ("d4") ("d5") ("d6") ("d7")
    ("a0") ("a1") ("a2") ("a3") ("a4") ("a5") ("a6") ("a7")
    ("pc") ("ccr") ("sr") ("ssp") ("usp")))


(defun munge-modifier-and-find-in-table (string table)
  (let ((modifier nil))
    (awhen (position #\. string)
      (setf modifier (subseq string (1+ it)))
      (setf string (subseq string 0 it)))
    (awhen (find string table :key #'car :test #'string-equal)
      (list string modifier))))

(defun register-p (string)
  (when (string-equal string "sp") (setf string "a7"))
  (munge-modifier-and-find-in-table string *asm-register-table*))

(defun opcode-p (string)
  (munge-modifier-and-find-in-table string *asm-opcode-table*))

(defun pseudo-op-p (string)
  (munge-modifier-and-find-in-table string *asm-pseudo-op-table*))


;;;; OPERAND CONSTRAINTS

(defun satisfies-operand-constraints-p (operands constraints)
  ;; if this is a string rather than a list, try matching operands
  ;; with that opcode instead of our current one.

  ;; if we reach the end of the list of operand constraints, these
  ;; must be invalid operands for the opcode.
)


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


(defun string-to-modifier (string)
  (cond ((string-equal string "b") 'byte)
	((string-equal string "w") 'word)
	((string-equal string "l") 'long)))


;;; XXX should this be maintained by the lexer instead?
(defvar *file-list* nil
  "Stack of file names that are actively open; checked by INCLUDE to
determine whether recursive nesting is occuring.")

(defun assemble (file)
  ;; create symbol table
  ;; create object file tmp (write header, start output section)
  ;; create backpatch list
  ;; open file and start processing
  (init-lexer file)
  ;; go through backpatch list, try to make all patches
  ;; finalize object file (write symbol table, patch header)
  )

(defun process-stream (stream)
  (handler-case
      (loop
       ;; line-counter
       (let ((line (parse (lambda () (next-token stream)))))
	 (assert (eql (pop line) 'line))
	 ;; we strip individual token position information here, which
	 ;; might suck for superfine debugging/warning precision, but
	 ;; we really don't need all that clutter.
	 ; (let ((position (extract-position-info line)))
	 ;   (strip-position-info line)

	 ;; if there's a label here, add it to the symbol table
	 ;;   (get-label)
	 ;; pseudo-ops care about labels, opcodes don't, so
	 ;; strip the label if it's a pseudo-op.

	 ;; separate opcode and operands
	 (format t "~&~%~A " (cadr (find 'operation line :key #'car)))
	 (dolist (x (extract-operands line))
	   (format t "~&~A " x))))
    (end-of-file)))

(defun output-data (data)
  "Outputs DATA to the currently active temporary object file and
updates the program counter.  Returns the address to which the data
assembled.")



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


;;;; Addressing mode simplifiers.

(defun simplify-immediate (tree)
  (assert (and (eql (car tree) 'immediate)
	       (eql (caadr tree) 'hash)))
  (list 'immediate (simplify-expression (third tree))))

(defun simplify-absolute (tree)
  (assert (eql (car tree) 'absolute))
  (list 'absolute (simplify-expression (second tree))))

(defun interpolate-registers (start end)
  (format t "~&interpolate registers from ~A to ~A"
	  (register-idx start :both-sets t)
	  (register-idx end :both-sets t))
  (loop for i from (register-idx start :both-sets t)
	to (register-idx end :both-sets t)
	collecting (list (car (aref *asm-register-table* i)))))

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
