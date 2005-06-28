;; -*- Lisp -*-

(defpackage #:assembler-system (:use #:cl #:asdf))
(in-package #:assembler-system)

(defsystem m68k-assembler
  :depends-on (:anaphora)
  :components
  ((:file "package")
   (:file "lexer" :depends-on ("package"))
   (:file "parser" :depends-on ("package"))
   (:file "assembler" :depends-on ("package" "lexer" "parser"))))

;;; XXX add stuff to build parser here
