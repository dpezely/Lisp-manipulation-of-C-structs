(defpackage :c-struct-system (:use :cl :asdf))
(in-package :c-struct-system)

(defsystem c-struct
  :name "Lisp manipulation of C structures"
  :author "Daniel Pezely <first name @ last name dot com>"
  :version "1"
  :description "Demonstration of sharing data between Lisp and C programs."
  :depends-on ( #+sbcl :sb-bsd-sockets
	       :cl-utilities)		; i.e., split-sequence
  :components
  ((:file "package")
   (:file "trivial-sockets" :depends-on ("package"))
   (:file "variables" :depends-on ("package"))
   (:file "utilities" :depends-on ("variables"))
   (:file "message" :depends-on ("variables" "utilities"))
   (:file "read" :depends-on ("message"))
   (:file "write" :depends-on ("message"))
   (:file "listen" :depends-on ("trivial-sockets" "message"))))
