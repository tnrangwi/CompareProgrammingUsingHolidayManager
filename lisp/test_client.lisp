#!/localhome/tnrangwi/bin/sbcl --script
(load "basetools.lisp")
(load "socket-service.lisp")
(defvar *server* (make-instance 'socket-service:socket-client :port 1970))
(format T "Initialised~%")
(format T "~A~%" (socket-service:call-server-function *server* "echo" "1" "2" "3"))
