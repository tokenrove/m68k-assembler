
(in-package :m68k-assembler)


(defvar *defining-macro-p* nil)
(defvar *defining-rept-p* nil)
(defvar *defining-conditional-compilation-p* nil)
(defvar *macro-buffer*)

(defvar *source-position*)
(defvar *last-label* nil "Last label seen by the assembler.  This is
used for generating the prefix for local labels.")
(defvar *backpatch-list* nil)

(defvar *defining-relocations-p* nil)

(defvar *sections* nil)
(defvar *current-section* nil "Current section we're assembling in.")
;; Things bound by new section.
(defvar *object-stream* nil "Stream to object data.")
(defvar *program-counter* nil "Program counter of current section.")
(defvar *relocation-table* nil "Relocations, indexed by PC.")
