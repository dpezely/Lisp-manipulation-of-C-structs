;;; message.lisp - process UDP datagram from Node

(in-package #:c-struct)

(defvar *max-datagram-length* 300
  "Value must match message.c version.
Smaller is better, despite contemporary limits of UDP packets being 64k.")

(defvar *app-name-length* 16
  "Value must match the message.c version")

(defparameter *agent-id-size*  2
  "Value must match sizeof(unsigned short) in C")

(defparameter *ctime-byte-size*  4
  "Value must match sizeof(uint32_t) in C for time_t.tv_sec")
(defparameter *ctime-bit-size*  (* 8 *ctime-byte-size*))


(defun make-datagram-buffer ()
  ;; use 'character rather than '(unsigned-byte 8) to aid debugging
  (make-array (+ *max-datagram-length* 24) :element-type 'character))


;; From the C code:
;; typedef struct _message {
;;   unsigned char magic_number;
;;   unsigned char version;
;;   unsigned char flags;
;;   unsigned char state;
;;
;;   char app_name[APP_NAME_LENGTH]; 
;;   unsigned short agent_id;        
;;   unsigned short other;           // UNUSED; for proper byte alignment
;;
;;   // all time values are in seconds and in network byte order
;;   uint32_t node_time;             // node's idea of "now"
;;   uint32_t app_time;              // e.g., start of App
;;   char status[STATUS_LENGTH];     // stats & counters page from App
;; } message_t;
(defparameter *magic-number-index* 0)
(defparameter *version-index*        (1+ *magic-number-index*))
(defparameter *flags-index*          (1+ *version-index*))
(defparameter *node-state-index*     (1+ *flags-index*))
(defparameter *app-name-start-index* (1+ *node-state-index*))
(defparameter *app-name-limit-index*  (+ *app-name-start-index* *app-name-length*))
(defparameter *agent-id-index*           *app-name-limit-index*)
(defparameter *other-index*           (+ *agent-id-index* *agent-id-size*))
(defparameter *node-time-index*       (+ *other-index* 2))
(defparameter *app-start-time-index*  (+ *node-time-index* *ctime-byte-size*))
(defparameter *status-start-index*    (+ *app-start-time-index* *ctime-byte-size*))
(defparameter *status-limit-index*       *max-datagram-length*)

(defparameter *status-length* (- *max-datagram-length* *status-start-index*)
  "Value must match the message.c version.
This is a MAXIMUM and not guarenteed to be actual length.")



(defun unpack-message (buffer &optional actual-length)
  (if (valid-message? buffer)
      (multiple-value-bind (app-name agent-id state flags node-time app-start-time status-text)
	  (extract-message-fields buffer (min *max-datagram-length*
					      actual-length))
	(format t "Preamble and version matched!~@
		   app name=~s~@
		   agent-id=~s~@
		   App start time: ~a==#x~8X~@
		   Node state=~s; flags=~s; time: ~a==#x~8X~@
		   status: ~a...~%"
		app-name
		agent-id
		app-start-time app-start-time
		state flags node-time node-time
		(subseq status-text 0 23)))
      (format t "INVALID message; ~a bytes:~a~%" actual-length
	      (dump-raw-message-fields buffer))))

(defun valid-message? (buffer)
  "Verify that the message is well formed."
  (let ((preamble-expected (char-code #\C))
	(preamble-received (elt buffer *magic-number-index*)))
    (unless (= preamble-expected preamble-received)
      (format t "WARNING: preamble expected ~s but received ~s~%"
	      preamble-expected preamble-received)
      (return-from valid-message?)))
  (let ((version-expected 241)
	(version-received (elt buffer *version-index*)))
    (unless (= version-expected version-received)
      (format t "WARNING: version expected ~s but received ~s~%"
	      version-expected version-received)
      (return-from valid-message?)))
  t)

(defun dump-raw-message-fields (buffer &optional stream)
  "Useful while writing/debugging reader or writer code"
  (format stream "
		 magic-number=~s
		 version=~s ~d
		 flags=~s
		 state=~s

		 app-name=~s
		 agent-id=~s
		 padding=~s

		 node-time=~s
		 app-time=~s
		 status=~s[...]~%"
	  (subseq buffer *magic-number-index* *version-index*)
	  (subseq buffer *version-index* *flags-index*)
	  (char-code (elt buffer *version-index*))
	  (subseq buffer *flags-index* *node-state-index*)
	  (subseq buffer *node-state-index* *app-name-start-index*)
	      
	  (subseq buffer *app-name-start-index* *app-name-limit-index*)
	  (subseq buffer *agent-id-index* *other-index*)
	  (subseq buffer *other-index* *node-time-index*)

	  (subseq buffer *node-time-index* *app-start-time-index*)
	  (subseq buffer *app-start-time-index* *status-start-index*)
	  (subseq buffer *status-start-index* (+ *status-start-index* 28))))

(defun extract-app-name (buffer)
  "get C string (null terminated and padded) from sequence"
  (subseq buffer *app-name-start-index*
	  (position-if #'(lambda (x)
			   (= x 0))
		       buffer
		       :start *app-name-start-index*)))

(defun extract-message-fields (buffer end)
   "Return values suitable for populating a Lisp structure
with INTERNed name of application mentioned in message.
We use INTERN so that Generic Methods may specialize on this value via EQ."
   (values (intern (string-downcase (extract-app-name buffer)))
	   (network-bytes-to-number buffer *agent-id-index* (* 8 *agent-id-size*))
	   (subseq buffer *node-state-index* (1+ *node-state-index*))
	   (subseq buffer *flags-index* (1+ *flags-index*))
	   (unix-to-lisp-time
	    (network-bytes-to-number buffer *node-time-index* *ctime-bit-size*))
	   (unix-to-lisp-time
	    (network-bytes-to-number buffer *app-start-time-index* *ctime-bit-size*))
	   (subseq buffer *status-start-index* (min *status-limit-index*
						    end))))

(defun dump-message (buffer &optional stream)
  "Render pretty-printed text version of buffer containing message via FORMAT."
  (let ((node-time (network-bytes-to-number buffer *node-time-index*
					    *ctime-bit-size*))
	(app-time  (network-bytes-to-number buffer *app-start-time-index*
					    *ctime-bit-size*)))
    (format stream "app name=~s ~@
		    App start time: ~a==#x~8X~@
		    Node state=~s; flags=~s; time: ~a==#x~8X~@
		    status: ~a...~%"
	    (extract-app-name buffer)
	    app-time app-time
	    (char-code (elt buffer *node-state-index*))
	    (char-code (elt buffer *flags-index*)) node-time node-time
	    (subseq buffer *status-start-index* (min (+ *status-start-index* 20)
						     (length buffer))))))
