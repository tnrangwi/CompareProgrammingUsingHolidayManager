#!/usr/bin/sbcl --script
(load "basetools.lisp")
(load "socket-service.lisp")
(load "holiday.lisp")
(format T "Startup...~%")
(holiday-manager:start-holiday-managing-server-script)
(format T "Shutting down...")
