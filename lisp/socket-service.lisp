; Author: Thorsten Rangwich
; See file LICENSE for copyright and usage of this code.
; This is a simple remote procedure server and client.
(require :sb-bsd-sockets)
					; FIXME: Free ports properly - does not seem to work, although I close anything.
					; FIXME: Unix does not send a proper CRLF in socket connections
					; possibly Windows does. Scan code for all #\Return and make windows tests
					; FIXME: Error handling for EOF.
(defpackage :socket-service
  (:use :cl :sb-bsd-sockets :basetools)
  (:export :socket-server :socket-client :push-handler :run-services-until-shutdown
	   :call-server-function :socket-function-error))

(in-package :socket-service)


;;; internal constants, not exported

(defconstant empty-char (code-char 0) "Empty value used to distinguish empty lines from end of input")


;;; Internal error conditions, not exported

(define-condition socket-function-internal-error (error) ((msg :initarg :msg :reader msg)))

;;; External function error conditions - exported: Use them to throw an application error from your socket function

(define-condition socket-function-error (socket-function-internal-error) ((msg :initarg :msg :reader msg)))


;;; internal helper functions, not exported

(defun make-stream-writer (fd sep)
  "Internal helper function. Creates a new function that writes a list result to the given stream.
Empty rows are marked accordingly, an EOF is marked using an empty line. So it is possible
to check if the result was complete just looking at the stream"
  (let ((stream-open 1))
    (lambda (&optional (row NIL row-supplied-p)) "Write one row to stream. If omitted, mark stream as closed"
      (if (eql stream-open 0) (error "Stream closed"))
      (cond
       (row-supplied-p
	(cond
	 (row (let ((line (make-array 0 :element-type 'character :fill-pointer 0 :adjustable T)))
		(format line "~A" (car row))
		(dolist (field (cdr row)) (if (stringp field) (format line "~C~A" sep field) (error "No string")))
		(format fd "~A~C~%" line #\Return)))
	 (T (format fd "~C~C~%" empty-char #\Return))))
       (T (format fd "~C~%" #\Return) (decf stream-open))))))


;;; generic functions


;; generic functions used in socket-server

(defgeneric push-handler (svr name func))
(defgeneric run-services-until-shutdown (svr))

;; generic functions used in client

(defgeneric call-server-function (svr fct-name &rest svr-args))


;;; class definitions with methods


;; interface class socket-server-interface

(defclass socket-server-interface ()
  ((addr :initarg :bindaddr :initform "127.0.0.1" :documentation "address to bind/connect to")
   (port :initarg :port :initform 1970 :documentation "port to bind/connect to")
   (field-separator :initarg :field-separator :initform #\| :reader fsep :documentation "Sockets stream field separator")
   (sock :documentation "internal usage, kind of socket file descriptor"))
  (:documentation "Base class providing socket and base interface for socket-client and server"))

(defmethod initialize-instance :after ((svr socket-server-interface) &key)
  "Initialize socket server or client. Currently only socket object needs to be created"
  (with-slots (sock addr) svr
					; convert IP address(es) or names
    
    ;(setf addr (make-inet-address addr))
					; FIXME: try ..catch errors here
    (setf addr (first (host-ent-addresses (get-host-by-name addr))))
    (setf sock (make-instance 'inet-socket :type :stream :protocol :tcp))))


;; socket-server

(defclass socket-server (socket-server-interface)
  ((storage :initform (make-instance 'attribute-collection) :reader shared-mem
	    :documentation "Shared memory for all handler functions")
   (service-functions :initform (make-hash-table :test #'equal) :documentation "hash containing function per service name")
   (privileged-addresses :initform NIL :initarg :privileged-addresses :documentation "List of priv. addr, converted in init")
   (services-run :initform NIL :documentation "Mark if services were run already. Only run once is allowed")
   (storage-init :initform NIL :initarg :init-shared-mem :documentation "Parameter to initialise memory has from"))
  (:documentation 
   "Socket server class. Create an instance, push compatible server functions and go
 to server mode with start-services"))

(defmethod initialize-instance :after ((svr socket-server) &key)
  "Initialize socket server. Currently binds address to get server ready to use"
  (with-slots (sock addr port privileged-addresses storage storage-init) svr
    (if (not (eq NIL storage-init))
	(setf storage (make-instance 'attribute-collection :init-from storage-init)))
					; Translate IP addresses to internal format
    (do ((pos privileged-addresses (setf pos (cdr pos)))) ((eq (car pos) NIL))
      (setf (car pos) (make-inet-address (car pos))))
    (format T "Privileged addresses:~A~%" privileged-addresses)
    (socket-bind sock addr port) ; bind to address(es)
    (socket-listen sock 5))) ; wait for incoming connection

(defmethod push-handler ((svr socket-server) (name string) (func function))
  "Add one handler function
FIXME: The function should be further analysed: It should accept a list as input
and a function taking only optional parameters (each representing a column) for output."
  (let ((funchash (slot-value svr 'service-functions)))
    (if (nth-value 1 (gethash name funchash)) (error "reset of service functions is not allowed"))
    (setf (gethash name funchash) func)))

(defmethod run-services-until-shutdown ((svr socket-server))

  "Builds up the connection and start serve using the mapped functions.
This can be shut down by sending signals (nyi), respecting a timeout and
using e.g. a monitoring file. If some of the remote functions produces
too much output this could be intercepted as well by the given function
to communicate with the socket.
FIXME: Error handling for e.g. EOF and signals."
  (format T "Run services...~%")
  (with-slots (sock service-functions storage services-run privileged-addresses) svr
	      (if services-run (error "Service was shutdown already, cannot restart"))
	      (do (conn remote-id stream (sep (fsep svr)) line service-request service-request-function priv)
		  (services-run)
		(format T "Wait for incoming connection...~%")
		(multiple-value-setq (conn remote-id) (socket-accept sock))
		(setf priv (dolist (m privileged-addresses NIL) (if (equalp m remote-id) (return T))))
		(format T "Incoming connection:~A,priv:~A~%" remote-id priv)
		(setf stream (socket-make-stream conn :output T :input T))
		(handler-case (let ()
				(setf line (read-line stream))
				(setf line (subseq line 0 (1- (length line))))
				(setf line (split-string line sep))
				(setf service-request (car line))
				(setf service-request-function (gethash (car line) service-functions))
				(cond 
				  (service-request-function
				   (funcall (gethash (car line) service-functions)
					    (cdr line)
					    (make-stream-writer stream sep)
					    storage
					    :priv priv))
				  ((and priv (equal (car line) "shutdown"))
				   (format T "...Shutdown requested.~%")
				   (funcall (make-stream-writer stream sep))
				   (setf services-run T))
				  (T
				   (format T "Function call '~A' ignored, privileged:~A.~%" (car line) priv)))
				(force-output stream)
				(close stream)
				) (end-of-file ()
				    (format T "Caller closed stream accidently while connected, disconnecting...~%")
				    (close stream))
				  (SB-INT:STREAM-DECODING-ERROR ()
				    (format T "Caller sent garbage in (possibly ^C), decoding error, disconnecting...~%")
				    (close stream))
				  (socket-function-internal-error (e)
				    (format T "Socket function failed:~A~%" (msg e))
				    (close stream))
				  (condition (e)
				    (format T "Unspecified general error:~A~%" e)
				    (handler-case (close stream) (T () (format T "Error closing stream (ignored)."))))
				  )
					; This may already be enough. I don't think it's necessary to close both.
					; Calling socket-close on a connection having a stream assigned
					; is redirected to do a close of the stream anyway.
		(socket-close conn))
	      (socket-close sock)))


;; socket-client

(defclass socket-client (socket-server-interface)
  ((fd-io :reader sstream))
  (:documentation "Client class for exchange with server using exactly one server interface"))

(defmethod initialize-instance :after ((clt socket-client) &key)
  "Initialize socket client: Connect and open stream. All instances created are already connected,
so do not just create them unless you wish to connect a server."
  (with-slots (fd-io sock addr port) clt
    (socket-connect sock addr port)
    (setf fd-io (socket-make-stream sock :input T :output T))))

(defmethod call-server-function ((svr socket-client) (fct string) &rest svr-args)

  "Call a server function named 'fct' on a connected socket, providing optional arguments.
Return list of result rows"

  (let (
	(cmdline (make-array 0 :element-type 'character :fill-pointer 0 :adjustable T))
	(fd (sstream svr))
	(sep (fsep svr))
	(res (cons NIL NIL)))

	(format cmdline "~A" fct)
	(dolist (i svr-args) (format cmdline "~C~A" sep i))
	(format fd "~A~C~%" cmdline #\Return)
	(force-output fd)
	(do ((pos res) line) (NIL)
	  (setf line (read-line fd))
	  (setf line (subseq line 0 (1- (length line)))) ; strips #\Return
					;(format T "Read line with ~A characters:~A~%" (length line) line)
	  (cond ((eql (length line) 0) (return (cdr res)))
		(T (setf (cdr pos) (cons NIL NIL)) (setf pos (cdr pos))))
					; if line is not marked as empty, fill result list with contents
	  (unless (eql (char line 0) empty-char) (setf (car pos) (split-string line sep))))))


;;; Simple example using server socket:
;;; Creates a socket, accepts the first connection attempt, echoe's one line read to stdout and
;;; writes back always the same string to the caller. Tested and even works in some way for windows.
;;; This even works without a change if loaded from any other package namespace. Just include
;;; the (require sb-bsd-sockets)

(defun socket-server-example (addr port)
  "Test function setting up a socket server, waiting for a connection, printing
out the first line, sending a useless response, dropping the connection and the socket.
Eaxmple: (socket-server-example #(127 0 0 1) 1270)"
  (let (sock conn stream line llen)
    (setf sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
    (sb-bsd-sockets:socket-bind sock addr port)
    (sb-bsd-sockets:socket-listen sock 1)
    (setf conn (sb-bsd-sockets:socket-accept sock))
    (setf stream (sb-bsd-sockets:socket-make-stream conn :output t :input t))
    (setf line (read-line stream))
    (setf llen (length line))
    (setf line (subseq line 0 (1- llen)))
    (format t "Got line with ~A characters:~A~%" llen line)
    (format stream "Response~C~%" #\Return)
    (force-output stream)
    (sb-bsd-sockets:socket-close conn)
    (sb-bsd-sockets:socket-close sock)))

;; Simple test client example.
(defun socket-client-example (addr port line)
  "Test function setting up a socket client. Sends one string
and prints result. Terminates it properly with <CR><LF>"
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)) res sstream)
    (sb-bsd-sockets:socket-connect sock addr port)
    (setf sstream (sb-bsd-sockets:socket-make-stream sock :input T :output T))
    (format sstream "~A~C~%" line #\Return)
    (force-output sstream)
    (setf res (read-line sstream))
    (format T "Got ~A characters" (length res))
    (if (eql (char res (1- (length res))) #\Return)
	(setf res (subseq res 0 (1- (length res))))
      (format T "Unterminated line without CR"))
    (format T "Got line with ~A characters:.~A.~%" (length res) res)
    (close sstream)
    (sb-bsd-sockets:socket-close sock)))

;;; Recipes

;; Address / protocol conversions
; (sb-bsd-sockets:make-inet-address "1.2.3.4")
; (sb-bsd-sockets:get-protocol-by-name "tcp")
; (sb-bsd-sockets:get-protocol-by-name "udp")


;; Small usage example for socket server using a state variable
; (defvar *server* (make-instance 'socket-service:socket-server :port 1974))
; 
; (socket-service:push-handler
;  *server* 
;  "echo" 
;  (lambda (argv fdfunc mem)
;    (let ((echo-counter (basetools:getattr mem "echo-counter")))
;      (if (eq echo-counter NIL) (setf echo-counter 0))
;      (incf echo-counter)
;      (format T "echo counter:~A~%" echo-counter)
;      (funcall fdfunc (list echo-counter))
;      (basetools:setattr mem "echo-counter" echo-counter)
;      (funcall fdfunc argv)
;      (funcall fdfunc))))
; 
; (socket-service:run-services-until-shutdown *server*)
