;; write.lisp

(in-package #:c-struct)

(defun demo-write (buffer &optional length (file-path "data.struct"))
  (unless length
    (setf length (length buffer)))
  (with-open-file (stream (merge-pathnames file-path)
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-exists :rename)
    (let ((written (length (write-sequence buffer stream))))
      (format t "wrote=~a bytes buffer=~s~%" written buffer)))
  buffer)

;;(demo-write (assemble-message))

#+(or)
(demo-write
 (assemble-message :app-name "OTHER APP" :agent-id 241
		   :state (char-code #\s) :flags (char-code #\f)
		   :app-time (unix-to-lisp-time 0)
		   :node-time (unix-to-lisp-time 1176702348)
		   :status-text "This is a test.
This is a test for Lisp manipulation of C structures.
Had this been a real application, there might be something actually useful here.
This is only a test.
"))


(defun demo-write-chars (buffer &optional length (file-path "data.struct"))
  "Less than optimal, but sometimes you really want character-by-character"
  (with-open-file (stream (merge-pathnames file-path)
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-exists :rename)
    (loop for i upto (or length (length buffer)) and byte = (elt buffer i)
       do (write-byte byte stream)
       finally (return-from demo-write-chars i))))
