;; write.lisp

(in-package #:c-struct)

(defun demo-write (buffer &optional length (file-path "data.struct"))
  "Fast because it may use OS's native write() but requires
byte-by-byte translation from 'character to 'unsigned-byte when
assembling the buffer as strings"
  (unless length
    (setf length (length buffer)))
  (with-open-file (stream (merge-pathnames file-path)
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-exists :rename)
    (let ((buffer (write-sequence
		   ;;byte-by-byte translation:
		   (map 'vector #'char-code buffer)
		   stream)))
      (format t "wrote=~a bytes buffer=~s~%" (length buffer) buffer)
      buffer)))

;;(demo-write (assemble-message))
#+(or)
(demo-write
 (assemble-message :app-name "OTHER APP" :agent-id 41
		   :state (char-code #\s) :flags (char-code #\f)
		   :app-time 2208988800
		   :status-text "This is a test.
This is a test for Lisp manipulation of C structures.
Had this been a real application, there might be something actually useful here.
This is only a test.
"))

#+DOES-NOT-WORK
(defun demo-write (buffer &optional length (file-path "data.struct"))
  ;; Simply reversing our #'demo-read function doesn't work!
  (unless length
    (setf length (length buffer)))
  (with-open-file (stream (merge-pathnames file-path)
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-exists :rename)
    (let ((buffer (write-sequence
		   ;;byte-by-byte translation
		   ;;fails at run-time due to MAP not being able to use
		   ;;UNSIGNED-BYTE as the target type:
		   (map '(unsigned-byte 8) #'char-code buffer) 
		   stream)))
      (format t "wrote=~a bytes buffer=~s~%" (length buffer) buffer)
      buffer)))
