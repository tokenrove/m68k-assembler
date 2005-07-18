;; -*- Lisp -*-

(defpackage #:assembler-system (:use #:cl #:asdf))
(in-package #:assembler-system)

(defsystem m68k-assembler
  :depends-on (:anaphora :cl-ppcre :osicat)
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "machine" :depends-on ("package"))
   (:file "lexer" :depends-on ("package" "machine"))
   (:file "parser" :depends-on ("package"))
   (:file "ast" :depends-on ("package"))
   (:file "sections" :depends-on ("package" "utils"))
   (:file "codegen" :depends-on ("package" "sections" "utils"))
   (:file "pseudo-ops" :depends-on ("package" "assembler"))
   (:file "assembler" :depends-on ("package" "sections" "utils"
				   "codegen" "ast" "parser" "lexer"))
   (:file "aout" :depends-on ("package" "assembler" "utils"))))

;;; XXX add stuff to build parser here
