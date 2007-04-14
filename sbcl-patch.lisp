;;; This is a patch to sbcl-1.0.3/contrib/sb-bsd-sockets/sockets.lisp

;;; Actual PATCH:
;; 210c210
;; <                   (t (loop for i from 0 below len
;; ---
;; >                   (t (loop for i from 0 below length

(in-package "SB-BSD-SOCKETS")

(defmethod socket-receive ((socket socket) buffer length
                           &key
                           oob peek waitall
                           (element-type 'character))
  (with-sockaddr-for (socket sockaddr)
    (let ((flags
           (logior (if oob sockint::MSG-OOB 0)
                   (if peek sockint::MSG-PEEK 0)
                   (if waitall sockint::MSG-WAITALL 0)
                   #+linux sockint::MSG-NOSIGNAL ;don't send us SIGPIPE
                   (if (eql (socket-type socket) :datagram)
                       sockint::msg-TRUNC 0))))
      (unless (or buffer length)
        (error "Must supply at least one of BUFFER or LENGTH"))
      (unless length
        (setf length (length buffer)))
      (when buffer (setf element-type (array-element-type buffer)))
      (unless (or (subtypep element-type 'character)
                  (subtypep element-type 'integer))
        (error "Buffer element-type must be either a character or an integer subtype."))
      (unless buffer
        (setf buffer (make-array length :element-type element-type)))
      ;; really big FIXME: This whole copy-buffer thing is broken.
      ;; doesn't support characters more than 8 bits wide, or integer
      ;; types that aren't (unsigned-byte 8).
      (let ((copy-buffer (sb-alien:make-alien (array (sb-alien:unsigned 8) 1) length)))
        (unwind-protect
            (sb-alien:with-alien ((sa-len sockint::socklen-t (size-of-sockaddr socket)))
              (let ((len
                     (sockint::recvfrom (socket-file-descriptor socket)
                                        copy-buffer
                                        length
                                        flags
                                        sockaddr
                                        (sb-alien:addr sa-len))))
                (cond
                  ((and (= len -1)
                        (member (sb-unix::get-errno)
                                (list sockint::EAGAIN sockint::EINTR)))
                   nil)
                  ((= len -1) (socket-error "recvfrom"))
                  (t (loop for i from 0 below length
                           do (setf (elt buffer i)
                                    (cond
                                      ((or (eql element-type 'character) (eql element-type 'base-char))
                                       (code-char (sb-alien:deref (sb-alien:deref copy-buffer) i)))
                                      (t (sb-alien:deref (sb-alien:deref copy-buffer) i)))))
                     (apply #'values buffer len (multiple-value-list
                                                 (bits-of-sockaddr socket sockaddr)))))))
          (sb-alien:free-alien copy-buffer))))))
