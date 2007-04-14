;; utilities.lisp

(in-package #:c-struct)

;; Note that use of ASCII codes violates the ANSI CL spec, as a Lisp
;; implementation is not guaranteed to use that character encoding.

(defun network-bytes-to-number (buffer start-index total-bits)
  "Convert network byte ordered sequence of unsigned bytes to a number."
  (let ((value 0))
    (loop for i downfrom (- total-bits 8) downto 0 by 8
       for cursor upfrom start-index
       do (setf value (dpb (char-code (elt buffer cursor))
			   (byte 8 i) value))
	 (format t "buffer[~d]==#x~2X; shift<< ~d bits; value=~d~%"
		 cursor (char-code (elt buffer cursor)) i value))
    value))

(defun number-to-network-bytes (number total-bits &optional buffer (start-index 0))
  "Convert number to network byte ordered sequence of unsigned bytes characters."
  (unless buffer
    (setf buffer (make-array (/ total-bits 8) :element-type 'character)))
  (loop for i downfrom (- total-bits 8) downto 0 by 8
     for cursor upfrom start-index
     do (setf (elt buffer cursor) (code-char (ldb (byte 8 i) number)))

       (let ((value (ldb (byte 8 i) number)))
	 (format t "number=~d: shift>> ~d bits; value=~d #x~2X; buffer[~d]==#x~2X~%"
		 number i value value cursor (char-code (elt buffer cursor)))))
  buffer)

;;(defvar now (get-universal-time))
;;(network-bytes-to-number (number-to-network-bytes now 32) 0 32)
;;(eql (network-bytes-to-number (number-to-network-bytes now 32) 0 32) now)


(defun print-date-and-time (&optional time stream)
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time (or time
				 (get-universal-time)))
    (format stream "~a, ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	    (nth weekday '("Monday" "Tuesday" "Wednesday" "Thursday"
			   "Friday" "Saturday" "Sunday"))
	    year month day  hour minute second)))


(let ((%unix-epoch% (encode-universal-time 0 0 0
					   1 1 1970
					   0)))
  (defun unix-to-lisp-time (unix-time)
    "Lisp and Unix have differing notions of time:
Universal time is an absolute time represented as a single
non-negative integer---the number of seconds since midnight,
January 1, 1900 GMT (ignoring leap seconds).
This is unlike Unix's gettimeofday() that is expressed in seconds
and microseconds since midnight (0 hour), January 1, 1970.
See HyperSpec/Body/25_adb.htm"
    (+ unix-time %unix-epoch%))

  (defun lisp-to-unix-time (&optional lisp-time)
    (- (or lisp-time
	   (get-universal-time))
       %unix-epoch%)))
    
(defun pretty-print-ip-address (address &optional (stream nil))
  (format stream "~d.~d.~d.~d"
	  (aref address 0) (aref address 1) (aref address 2) (aref address 3)))
