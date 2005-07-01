;;; Tables and things which are truly machine (m68k) specific.
;;;
;;; Julian Squires/2005

(in-package :m68k-assembler)

(defparameter *asm-opcode-table*
  ;; first field is the opcode name, sans modifiers.  remaining fields
  ;; are either pairs of operand constraints and code generation
  ;; masks, or functions.  these are matched or called in sequence,
  ;; until one can be found which matches.
  ;;
  ;; Code generation masks are lists in the form: (size code) where
  ;;  size indicates the number of bits this part should take up, and
  ;;  code is the code to evaluate when generating this part of the
  ;;  expression.  (Actually, code should be a function call, now.)
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
	    (((byte word long) (all-ea-modes) (data-register))
	     ((4 ,(if (string= x "ADD") #b1101 #b1001))
	      (3 (register-idx second-operand)) (1 #b1)
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode first-operand))
	      (? (effective-address-extra first-operand modifier))))
	    (((byte word long) (data-register) (memory-alterable-modes))
	     ((4 ,(if (string= x "ADD") #b1101 #b1001))
	      (3 (register-idx first-operand)) (1 #b0)
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (effective-address-extra second-operand modifier))))
	    ,(concatenate 'string x "A")
	    ,(concatenate 'string x "I")
	    ,(concatenate 'string x "Q")
	    ,(concatenate 'string x "X"))
	   (,(concatenate 'string x "A")
	    (((word long) (all-ea-modes) (address-register))
	     ((4 ,(if (string= x "ADD") #b1101 #b1001))
	      (3 (register-idx second-operand))
	      (1 (if-eql-word-p modifier 0 1))
	      (2 #b11) (6 (effective-address-mode first-operand))
	      (? (effective-address-extra first-operand modifier)))))
	   (,(concatenate 'string x "I")
	    (((byte word long) (immediate) (data-alterable-modes))
	     ((8 ,(if (string= x "ADD") #b00000110 #b00000100))
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (immediate-value first-operand modifier))
	      (? (effective-address-extra second-operand modifier)))))
	   (,(concatenate 'string x "Q")
	    (((byte word long) (immediate (<= 1 value 8)) (alterable-modes))
	     ((4 #b0101) (3 (immediate-value first-operand))
	      (1 ,(if (string= x "ADD") 0 1))
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (effective-address-extra second-operand modifier)))))
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
	      `((((byte word long) (data-addressing-modes) (data-register))
		 ((4 ,(cond ((string= x "AND") #b1100)
			    ((string= x "OR") #b1000)))
		  (3 (register-idx second-operand)) (1 #b0)
		  (2 (modifier-bits modifier))
		  (6 (effective-address-mode first-operand))
		  (? (effective-address-extra first-operand modifier))))))
	    (((byte word long) (data-register) (memory-alterable-modes))
	     ((4 ,(cond ((string= x "AND") #b1100)
			((string= x "EOR") #b1011)
			((string= x "OR") #b1000)))
	      (3 (register-idx first-operand)) (1 #b1)
	      (2 (modifier-bits modifier))
	      (6 (effective-address-mode second-operand))
	      (? (effective-address-extra second-operand modifier))))
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
	      (? (effective-address-extra second-operand modifier))))
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
	   (((byte word long) (immediate (<= 1 value 8)) (data-register))
	    ((4 #b1110) (3 (immediate-value first-operand))
	     (1 ,(if (string= x "L" :start1 (1- (length x))) 1 0))
	     (2 (modifier-bits modifier))
	     (3 ,(cond ((string= x "LS" :end1 2) #b101)
		       ((string= x "AS" :end1 2) #b100)
		       ((string= x "ROX" :end1 3) #b110)
		       (t #b111)))
	     (3 (register-idx second-operand))))
	   (((byte word long) (memory-alterable-modes))
	    ((5 #b11100)
	     (2 ,(cond ((string= x "LS" :end1 2) #b01)
		       ((string= x "AS" :end1 2) #b00)
		       ((string= x "ROX" :end1 3) #b10)
		       (t #b11)))
	     (1 ,(if (string= x "L" :start1 (1- (length x))) 1 0))
	     (2 #b11) (6 (effective-address-mode first-operand))
	     (? (effective-address-extra first-operand modifier))))))
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
	     (? (effective-address-extra second-operand modifier))))
	   (((byte long) (immediate) (data-alterable-modes))
	    ((8 #b00001000) (2 ,(cdr x))
	     (6 (effective-address-mode second-operand))
	     (8 #b00000000) (8 (immediate-value immediate 'byte))
	     (? (effective-address-extra second-operand modifier))))))
       '(("BCHG" . #b01) ("BCLR" . #b10) ("BSET" . #b11) ("BTST" . #b00)))

    ("CHK"
     (((word) (data-addressing-modes) (data-register))
      ((4 #b0100) (3 (register-idx second-operand)) (3 #b110)
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))
    
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
	     (6 (effective-address-mode first-operand))
	     (? (effective-address-extra first-operand modifier))))))
       '("CLR" "NEG" "NEGX" "NOT"))

    ("CMP"
     (((byte word long) (all-ea-modes) (data-register))
      ((4 #b1011) (3 (register-idx second-operand)) (1 #b0)
       (2 (modifier-bits modifier))
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier))))
     "CMPA" "CMPI" "CMPM")
    ("CMPA"
     (((word long) (all-ea-modes) (address-register))
      ((4 #b1011) (3 (register-idx second-operand))
       (3 (if-eql-word-p modifier #b011 #b111))
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))
    ("CMPI"
     (((byte word long) (immediate) (data-alterable-modes))
      ((8 #b00001100) (2 (modifier-bits modifier))
       (6 (effective-address-mode second-operand))
       (? (immediate-value first-operand modifier))
       (? (effective-address-extra second-operand modifier)))))
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
	     (? (effective-address-extra first-operand modifier))))))
       '("DIVS" "DIVU" "MULS" "MULU"))

    ("EXG"
     (((long) (data-register) (data-register))
      ((4 #b1100) (3 (register-idx first-operand))
       (6 #b101000) (3 (register-idx second-operand))))
     (((long) (address-register) (address-register))
      ((4 #b1100) (3 (register-idx first-operand))
       (6 #b101001) (3 (register-idx second-operand))))
     (((long) (data-register) (address-register))
      ((4 #b1100) (3 (register-idx first-operand))
       (6 #b110001) (3 (register-idx second-operand)))))

    ("EXT"
     (((word long) (data-register))
      ((7 #b0100100) (3 (if-eql-word-p modifier #b010 #b011))
       (3 #b000) (3 (register-idx first-operand)))))

    ("ILLEGAL" (nil ((16 #b0100101011111100))))

    ("JMP"
     ((nil (control-addressing-modes))
      ((10 #b0100111011) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))
    ("JSR"
     ((nil (control-addressing-modes))
      ((10 #b0100111010) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))

    ("LEA"
     (((long) (control-addressing-modes) (address-register))
      ((4 #b0100) (3 (register-idx second-operand)) (3 #b111)
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))

    ("LINK"
     ((nil (address-register) (immediate))
      ((13 #b0100111001010) (3 (register-idx first-operand))
       (16 (immediate-value immediate)))))

    ("MOVE"
     (((byte word long) (all-ea-modes) (data-alterable-modes))
      ((2 #b00) (2 (modifier-bits modifier))
       (6 (effective-address-mode second-operand :flipped-p t))
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier))
       (? (effective-address-extra second-operand modifier))))
     (((word) (register (string-equal value "CCR"))
       (data-alterable-modes))
      ((10 #b0100001011) (6 (effective-address-mode second-operand))
       (? (effective-address-extra second-operand modifier))))
     (((word) (data-addressing-modes) (register (string-equal value "CCR")))
      ((10 #b0100010011) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier))))
     (((word) (data-addressing-modes) (register (string-equal value "SR")))
      ((10 #b0100011011) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier))))
     (((word) (register (string-equal value "SR")) (data-alterable-modes))
      ((10 #b0100000011) (6 (effective-address-mode second-operand))
       (? (effective-address-extra second-operand modifier))))
     (((long) (register (string-equal value "USP")) (address-register))
      ((13 #b0100111001101) (3 (register-idx second-operand))))
     (((long) (register (string-equal value "USP")) (address-register))
      ((13 #b0100111001101) (3 (register-idx second-operand))))
     "MOVEA" "MOVEQ")
    ("MOVEA"
     (((word long) (all-ea-modes) (address-register))
      ((2 #b00) (2 (modifier-bits modifier))
       (3 (register-idx second-operand)) (3 #b001)
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))
    ("MOVEC") ;; XXX I don't understand the syntax for this one.
    ("MOVEM"
     (((word long) (register-list) (movem-pre-modes))
      ((9 #b010010001) (1 (if-eql-word-p modifier 0 1))
       (6 (effective-address-mode second-operand))
       (16 (register-list-mask first-operand :flipped-p t))
       (? (effective-address-extra second-operand modifier))))
     (((word long) (movem-post-modes) (register-list))
      ((9 #b010011001) (1 (if-eql-word-p modifier 0 1))
       (6 (effective-address-mode first-operand))
       (16 (register-list-mask second-operand :flipped-p nil))
       (? (effective-address-extra first-operand modifier)))))
    ("MOVEP"
     (((word long) (data-register) (displacement-indirect))
      ((4 #b0000) (3 (register-idx first-operand))
       (3 (if-eql-word-p modifier #b110 #b111)) (3 #b001)
       (3 (register-idx (indirect-base-register second-operand)))))
     (((word long) (displacement-indirect) (data-register))
      ((4 #b0000) (3 (register-idx second-operand))
       (3 (if-eql-word-p modifier #b100 #b101)) (3 #b001)
       (3 (register-idx (indirect-base-register first-operand))))))
    ("MOVEQ"
     (((long) (immediate (<= 0 value 255)) (data-register))
      ((4 #b0111) (3 (register-idx second-operand)) (1 #b0)
       (8 (immediate-value first-operand)))))
    ;; XXX MOVES (68010 instruction)

    ("NBCD"
     (((byte) (data-alterable-modes))
      ((10 #b0100100000) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))

    ("RESET" (nil ((16 #b0100111001110000))))
    ("NOP"   (nil ((16 #b0100111001110001))))
    ("STOP"  (nil ((16 #b0100111001110010))))
    ("RTE"   (nil ((16 #b0100111001110011))))
    ("RTD"   (nil ((16 #b0100111001110100))))
    ("RTS"   (nil ((16 #b0100111001110101))))
    ("TRAPV" (nil ((16 #b0100111001110110))))
    ("RTR"   (nil ((16 #b0100111001110111))))

    ("PEA"
     (((long) (control-addressing-modes))
      ((10 #b0100100001) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))

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

    ("SWAP"
     (((word) (data-register))
      ((13 #b0100100001000) (3 (register-idx first-operand)))))

    ("TAS"
     (((byte) (data-alterable-modes))
      ((10 #b0100101011) (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))

    ("TRAP"
     ((nil (immediate (<= 0 value 15)))
      ((12 #b010011100100) (4 (immediate-value first-operand)))))

    ("TST"
     (((byte word long) (data-alterable-modes))
      ((8 #b01001010) (2 (modifier-bits modifier))
       (6 (effective-address-mode first-operand))
       (? (effective-address-extra first-operand modifier)))))

    ("UNLK"
     ((nil (address-register))
      ((13 #b0100111001011) (3 (register-idx first-operand)))))))


;; The order of elements in this table are significant.
(defparameter *asm-register-table*
  #(("d0") ("d1") ("d2") ("d3") ("d4") ("d5") ("d6") ("d7")
    ("a0") ("a1") ("a2") ("a3") ("a4") ("a5") ("a6") ("a7")
    ("pc") ("ccr") ("sr") ("ssp") ("usp")))


;;;; HELPER FUNCTIONS

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

(defun string-to-modifier (string)
  (cond ((string-equal string "b") 'byte)
	((string-equal string "w") 'word)
	((string-equal string "l") 'long)))


;;; Really dumb, but this was the only thing in the code generation
;;; table that needed to be changed to a function call in order to
;;; stop using eval in the code generator.
(defun if-eql-word-p (modifier a b)
  (if (eql modifier 'word) a b))