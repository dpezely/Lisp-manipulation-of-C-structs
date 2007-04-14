;;; variables.lisp

(in-package #:c-struct)

(defparameter *datagram-bind-address* nil "IP network interface on which we listen")
(defparameter *datagram-port-number* 8989 "IP port number on which we listen")
