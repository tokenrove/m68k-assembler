
(in-package :m68k-assembler)

(defparameter *asm-opcode-table*
  #(("MOVE.B" code)
    ("MOVE.W" code)
    ("MOVE.L" code)))
(defparameter *asm-pseudo-op-table*
  #(("SECTION" code)
    ("INCLUDE" code)))