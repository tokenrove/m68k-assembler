;; -*- Lisp -*-

(defpackage #:assembler-system (:use #:cl #:asdf))
(in-package #:assembler-system)

(defsystem m68k-assembler
  :depends-on (:anaphora :cl-ppcre :osicat)
  :components
  ((:file "package")
   (:file "special-variables" :depends-on ("package"))
   (:file "utils" :depends-on ("package"))
   (:file "machine" :depends-on ("package" "utils"))
   (:file "lexer" :depends-on ("package" "machine" "utils"))
   (:file "parser" :depends-on ("package"))
   (:file "ast" :depends-on ("package" "utils" "lexer"))
   (:file "sections" :depends-on ("package" "utils" "special-variables"))
   (:file "symbol-table" :depends-on ("package" "utils" "sections" "special-variables"))
   (:file "codegen" :depends-on ("package" "sections" "utils"
				 "special-variables" "machine"))
   (:file "pseudo-ops" :depends-on ("package" "assembler" "special-variables"
			            "symbol-table"))
   (:file "assembler" :depends-on ("package" "sections" "utils"
				   "codegen" "ast" "parser" "lexer"
				   "symbol-table" "special-variables"))
   (:file "aout" :depends-on ("package" "assembler" "utils" "symbol-table"))))

;;; XXX add stuff to build parser here
