
(in-package :m68k-assembler)

(defun tokenize-line (line)
  (if ()))
;; start reading a line...
;;   until we find whitespace,
;;     read a label.
;;   strip : from label if it was there.
;;   push label.
;;   skip whitespace.
;;   read op until whitespace.
;;   determine whether op is pseudo or opcode.
;;   push op.
;;   read and push operands until end of line.
;;   push $.
