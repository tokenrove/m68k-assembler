;; -*- Lisp -*-

(defpackage #:assembler-system (:use #:cl #:asdf))
(in-package #:assembler-system)

(defsystem m68k-assembler
  :depends-on (:anaphora :cl-ppcre)
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "machine" :depends-on ("package"))
   (:file "lexer" :depends-on ("package"))
   (:file "parser" :depends-on ("package"))
   (:file "ast" :depends-on ("package"))
   (:file "codegen" :depends-on ("package"))
   (:file "assembler" :depends-on ("package"))))

;;; XXX add stuff to build parser here
