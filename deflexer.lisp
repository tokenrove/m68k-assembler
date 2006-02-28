;;; deflexer using cl-ppcre
;;;
;;; Heavily stolen from
;;;  http://common-lisp.net/pipermail/cl-ppcre-devel/2004-June/000041.html
;;; by Edi Weitz, combined with other stuff found in the CL-PPCRE code.
;;;
;;; Hacked together by Julian Squires <julian@cipht.net> / 2006.
;;; XXX Needs heavy refactoring!

(defpackage :cl-ppcre-lex
  (:use :cl :cl-ppcre :anaphora)
  (:import-from :cl-ppcre #:nsubseq)
  (:export #:deflexer))

(in-package :cl-ppcre-lex)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-unique-names ((&rest bindings) &body body)
    ;; see <http://www.cliki.net/Common%20Lisp%20Utilities>
    `(let ,(mapcar #'(lambda (binding)
		       (check-type binding (or cons symbol))
		       (if (consp binding)
			   (destructuring-bind (var x) binding
			     (check-type var symbol)
			     `(,var (gensym ,(etypecase x
							(symbol (symbol-name x))
							(character (string x))
							(string x)))))
			   `(,binding (gensym ,(symbol-name binding)))))
		   bindings)
      ,@body))

  (defun collect-bindings (var-list string reg-starts reg-ends)
    (loop for var in var-list
	  for counter from 0
	  when var
	  collect `(,var (awhen (aref ,reg-starts ,counter)
			   (nsubseq ,string it
				    (aref ,reg-ends ,counter))))))

  (defun gather-fns (list)
    (with-unique-names (scanner string start match-start match-end
	                reg-starts reg-ends next-pos-fn)
      (loop for x in list
	    collect
	    (destructuring-bind (regex (&rest var-list) &body body) x
	      (let ((bindings (collect-bindings var-list string reg-starts reg-ends)))
		`(cons ,regex
		  (lambda (,scanner ,string ,start ,next-pos-fn)
		    (multiple-value-bind
			  (,match-start ,match-end ,reg-starts ,reg-ends)
			(cl-ppcre:scan ,scanner ,string :start ,start)
		      ,@(unless bindings
				`((declare (ignore ,reg-starts ,reg-ends))))
		      (when ,match-start
			(let ((it (progn ,@(if bindings
					       `((let (,@bindings) ,@body))
					       body))))
			  (when it (funcall ,next-pos-fn ,match-end) it))))))))))))

(defmacro deflexer (name &body body)
  (with-unique-names (regex-table regex sexpr-regex anchored-regex function)
    `(let ((,regex-table
	    (loop for (,regex . ,function) in (list ,@(gather-fns body))
		  for ,sexpr-regex =
		  (etypecase ,regex
		    (function
		     (error "Compiled scanners are not allowed here"))
		    (string
		     (cl-ppcre::parse-string ,regex))
		    (list
		     ,regex))
		  for ,anchored-regex =
		  (cl-ppcre:create-scanner `(:sequence
					     :modeless-start-anchor
					     ,,sexpr-regex))
		  collect (cons ,anchored-regex ,function))))
      (defun ,name (string next-pos-fn &key ((:start start) 0))
	(loop for (scanner . function) in ,regex-table
	      for value = (funcall function scanner string start next-pos-fn)
	      when value do (return value))))))
