(in-package :m68k-assembler)

(defun bit-vector->int (vector)
  "Converts a bit-vector to an integer, assuming that array indices
correspond to bit places in the integer.  (For example, index 0 in
VECTOR corresponds to the least-significant bit of the return value.)"
  (do ((value 0 (+ (ash value 1) (aref vector i)))
       (i (1- (length vector)) (1- i)))
      ((< i 0) value)))

;;; XXX probably totally fucked in this modern world of Unicode.
(defun string->int (string)
  "Converts a string to an integer, assuming that character elements
correspond to bytes, and that they are limited in range from 0 to
255."
  (do ((value 0 (+ (ash value 8) (char-code (char string i))))
       (i 0 (1+ i)))
      ((>= i (length string)) value)))

(defun carat (x) (if (consp x) (car x) x))


(defun write-big-endian-data (stream data length)
  (assert (zerop (logand length 7)))
  (do ((pos (- length 8) (- pos 8)))
      ((< pos 0))
    (write-byte (ldb (byte 8 pos) data) stream)))

