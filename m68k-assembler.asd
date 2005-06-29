;; -*- Lisp -*-

(defpackage #:assembler-system (:use #:cl #:asdf))
(in-package #:assembler-system)

(defsystem m68k-assembler
  :depends-on (:anaphora)
  :components
  ((:file "package")
   (:file "lexer" :depends-on ("package"))
   (:file "parser" :depends-on ("package"))
   (:file "ast" :depends-on ("package"))
   (:file "codegen" :depends-on ("package"))
   (:file "assembler" :depends-on ("package"))))

;;; XXX add stuff to build parser here
