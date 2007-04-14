;;; trivial-sockets.lisp - borrowed/forked from http://CLiki.net/trivial%20sockets

(in-package #:c-struct)

;; you're using a part of the interface that the implementation doesn't do
(define-condition unsupported (error) 
  ((feature :initarg :feature :reader unsupported-feature)))

;; all-purpose error: host not found, host not responding,
;; no service on that port, etc
(define-condition socket-error (error)
  ((nested-error :initarg :nested-error :reader socket-nested-error)))

(defun resolve-hostname (name)
  (cond
    ((eql name :any)  #(0 0 0 0))
    ((typep name '(vector * 4)) name)
    (t (car (sb-bsd-sockets:host-ent-addresses
	     (sb-bsd-sockets:get-host-by-name name))))))





;; Steps involved in establishing a TCP "server" socket: (i.e., listening)
;;    1. Create socket; in C on Unix, use socket() system call.
;;    2. Bind socket to an address; in C/Unix, use bind() system call.
;;       (INET domain sockets require an IP address number and port
;;       number, which may be 0.0.0.0 and 0 as wildcards.)
;;    3. Listen for connections; in C/Unix, use listen().
;;    4. Accept a connection; in C/Unix, use accept().
;;       (This typically blocks until a client connects; in C/Unix,
;;       use select(), but CMUCL and SBCL both use an event system.
;;       After all, Lisp predates Unix.)
;;    5. Send and receive data; in C/Unix, use recv() family of calls.
;;       (These effectively allow nonblocking i/o via polling.)
;; Differences for a UDP "server" socket:
;;    Omit step 2 because datagrams are connectionless, so nothing to bind.
;;    But don't worry: should you forget, you'll an OS error.

(defun open-server (&key (host :any) (port 0)
		    (reuse-address t)
		    (backlog 1)
		    (type :stream)
		    (protocol :tcp))
  "Returns a server socket"
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket 
			       :type type
			       :protocol protocol)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (sb-bsd-sockets:socket-bind  socket (resolve-hostname host) port)
    (when (eq protocol :tcp)
      (sb-bsd-sockets:socket-listen socket backlog))
    socket))

(defmacro with-server ((socket arguments) &body forms)
  `(let (,socket)
    (unwind-protect 
	 (progn
	   (setf ,socket (open-server ,@arguments))
	   ,@forms)
      (if ,socket
	  (sb-bsd-sockets:socket-close ,socket)))))

