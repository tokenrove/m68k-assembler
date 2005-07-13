(in-package :m68k-assembler)

;;;; LISTS

(defun carat (x)
  "If X is a cons, return the car of it.  Otherwise, return X."
  (if (consp x) (car x) x))


;;;; BINARY DATA, STREAMS

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
correspond to bytes, that they are limited in range from 0 to 255, and
that they are stored from greatest value to least (big-endian)."
  (do ((value 0 (+ (ash value 8) (char-code (char string i))))
       (i 0 (1+ i)))
      ((>= i (length string)) value)))

(defun read-big-endian-data (stream length)
  "Read LENGTH bits of data encoded big-endian from STREAM, returning
an integer.  LENGTH must be a multiple of 8."
  (assert (zerop (logand length 7)))
  (do ((pos (- length 8) (- pos 8))
       (value (read-byte stream) (logior (read-byte stream)
					 (ash value 8))))
      ((<= pos 0) value)))

(defun write-big-endian-data (stream data length)
  "Write LENGTH bits of the integer DATA to STREAM, in big-endian
order.  LENGTH must be a multiple of 8."
  (assert (zerop (logand length 7)))
  (do ((pos (- length 8) (- pos 8)))
      ((< pos 0))
    (write-byte (ldb (byte 8 pos) data) stream)))

(defun copy-stream-contents (source destination
			     &key (element-type 'unsigned-byte))
  "Copy all data from open stream SOURCE to open stream DESTINATION.
SOURCE is positioned at its beginning, and read until it reaches the
end of file."
  (file-position source 0)
  (let ((buffer (make-array '(4096) :element-type element-type)))
    (do ((bytes #1=(read-sequence buffer source) #1#))
	((= bytes 0))
      (write-sequence buffer destination :end bytes))))
