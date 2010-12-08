#!/usr/bin/sbcl --script
; Author: Thorsten Rangwich
; See file LICENSE for copyright and usage of this code.
; This is a startup script for the holiday server.
(load "basetools.lisp")
(load "socket-service.lisp")
(load "holiday.lisp") ; prerequisites are not yet defined properly. That's why they are loaded first.
(format T "Startup...~%")
(holiday-manager:start-holiday-managing-server-script)
(format T "Shutting down...")
