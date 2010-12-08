#!/localhome/tnrangwi/bin/sbcl --script
(load "basetools.lisp")
(load "socket-service.lisp")
(defvar *server* (make-instance 'socket-service:socket-server :privileged-addresses '("127.0.0.1") :bindaddr "127.0.0.1" :port 1971))

(socket-service:push-handler
 *server* 
 "echo" 
 (lambda (argv fdfunc mem &key &allow-other-keys)
   (let ((echo-counter (basetools:getattr mem "echo-counter")))
     (if (eq echo-counter NIL) (setf echo-counter 0))
     (incf echo-counter)
     (format T "echo counter:~A~%" echo-counter)
     ; (funcall fdfunc (list (format NIL "~A" echo-counter)))
     (funcall fdfunc (list echo-counter))
     (basetools:setattr mem "echo-counter" echo-counter)
     (funcall fdfunc argv)
     (funcall fdfunc))))

(socket-service:run-services-until-shutdown *server*)
(format T "finished!")
