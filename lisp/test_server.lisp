#!/localhome/tnrangwi/bin/sbcl --script
; Author: Thorsten Rangwich
; See file LICENSE for copyright and usage of this code.
; This tests the socket server. One function is pushed to
; the server and then the server is run.

; FIXME: There should be a require here instead. Test with sbcl how to get this solved in this prototype.
(load "basetools.lisp")
(load "socket-service.lisp")
(defvar *server* (make-instance 'socket-service:socket-server :privileged-addresses '("127.0.0.1") :bindaddr "127.0.0.1" :port 1971))

(socket-service:push-handler
 *server* ; the socket server, where to add the function
 "echo" ; the server's name of the function
 (lambda (argv fdfunc mem &key &allow-other-keys)  ; well, the function itself. The signature is documented in the interface.
   (let ((echo-counter (basetools:getattr mem "echo-counter")))
     (if (eq echo-counter NIL) (setf echo-counter 0))
     (incf echo-counter)
     (format T "echo counter:~A~%" echo-counter)
     ; (funcall fdfunc (list (format NIL "~A" echo-counter)))
     (funcall fdfunc (list echo-counter))
     (basetools:setattr mem "echo-counter" echo-counter)
     (funcall fdfunc argv)
     (funcall fdfunc))))

(socket-service:run-services-until-shutdown *server*) ; effectively now run the socket server, with this one function added.
(format T "finished!") ; something made the server shut down.
