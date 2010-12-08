#!/localhome/tnrangwi/bin/sbcl --script
; Author: Thorsten Rangwich
; See file LICENSE for copyright and usage of this code.
; This file tests the client interface.
; It assumes a server with a echo function. The test server has one.
(load "basetools.lisp")
(load "socket-service.lisp")
(defvar *server* (make-instance 'socket-service:socket-client :port 1970))
(format T "Initialised~%")
(format T "~A~%" (socket-service:call-server-function *server* "echo" "1" "2" "3"))
