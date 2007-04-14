;; listen.lisp -- receive network messages

(in-package :c-struct)

(defun demo-udp (&optional (max-length *max-datagram-length*) (wait-time 30))
  "works on MacOSX sbcl 1.0"
  (let ((buffer (make-array max-length :element-type 'character)))
    (with-server (socket (:port 8989 :type :datagram :protocol :udp))
      (sb-sys:with-fd-handler ((sb-bsd-sockets:socket-file-descriptor socket)
			       :input #'(lambda (fd)
					  (declare (ignore fd))
					  (multiple-value-bind (buffer length)
					      (sb-bsd-sockets:socket-receive socket buffer max-length)
					    (format t "received=~a max=~a buffer=~s~%"
						    length max-length buffer)
					    (unpack-message buffer length))))
	(format t "waiting...~%")(force-output) ;delete me
	(sb-sys:serve-event wait-time)))))

(defun demo-tcp (&optional (max-length *max-datagram-length*) (wait-time 30))
  (let ((buffer (make-array max-length :element-type 'character)))
    (with-server (socket (:port 8989))
      (sb-sys:with-fd-handler ((sb-bsd-sockets:socket-file-descriptor socket)
			       :input #'(lambda (fd)
					  (declare (ignore fd))
					  (let ((conn (sb-bsd-sockets:socket-accept socket)))
					    (multiple-value-bind (buffer length)
						(sb-bsd-sockets:socket-receive conn buffer max-length)
					      (format t "received=~a max=~a buffer=~s~%"
						      length max-length buffer)
					      (unpack-message buffer length)))))
	(format t "waiting...~%")(force-output) ;delete me
	(sb-sys:serve-event wait-time)))))

;; 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456 100 23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456 200 234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
