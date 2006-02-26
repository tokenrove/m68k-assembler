(defpackage #:m68k-assembler
  (:nicknames #:m68k-asm)
  (:use #:cl #:anaphora)
  (:export #:assemble))

(defpackage #:m68k-assembler-tests
  (:use #:cl #:rt))