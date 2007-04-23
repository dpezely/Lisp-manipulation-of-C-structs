;; read.lisp

(in-package #:c-struct)

(defun demo-read (&optional (file-path "data.struct") (max-length *max-datagram-length*))
  "Should use OS level block read and therefore should be fast."
  (with-open-file (stream (merge-pathnames file-path)
			  :element-type '(unsigned-byte 8)
			  :direction :input)
    (let ((buffer (make-array max-length :element-type '(unsigned-byte 8) :fill-pointer t)))
      (let ((actual-length (read-sequence buffer stream :end max-length)))
	 (setf (fill-pointer buffer) actual-length)
	 (format t "received=~a max=~a buffer=~s~%" actual-length max-length buffer)
	 (unpack-message buffer actual-length)
	 (values buffer actual-length)))))

#+(or)
(defun demo-read-chars (&optional (file-path "data.struct") (max-length *max-datagram-length*))
  "Less than optimal, but sometimes you really need character-by-character"
  (with-open-file (stream (merge-pathnames file-path)
			  :element-type '(unsigned-byte 8)
			  :direction :input)
    (loop
       with buffer = (make-array max-length :element-type '(unsigned-byte 8) :fill-pointer t)
       for i upto max-length
       and byte = (read-byte stream nil 'eof)
       until (eql byte 'eof)
       do (setf (elt buffer i) byte)
       finally
	 (setf (fill-pointer buffer) i)
	 (format t "received=~a max=~a buffer=~s~%" i max-length buffer)
	 (unpack-message buffer i)
	 (return-from demo-read-chars (values buffer i)))))
