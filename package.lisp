(in-package #:cl-user)

;(load "sbcl-patch")

(defpackage #:c-struct
  (:use #:cl)
  (:export #:*datagram-bind-address*
	   #:*datagram-port-number*
	   #:demo-udp
	   #:demo-tcp))
