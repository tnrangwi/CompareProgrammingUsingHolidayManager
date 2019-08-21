;;; Author: Thorsten Rangwich
;;; See file LICENSE for copyright and usage of this code.
;;;
;;; holiday-manager package
;;;
;;; Implements old interface already implemented in Perl and Python.
;;; Files now should be stored in a more flexible format, but I stay compatible
;;; to the old implementations for backtesting.

;(require :socket-service :basetools) ; Well, I did not get this up and running yet. sbcl is a bit complicated with that.

(defpackage holiday-manager
  (:use :cl :socket-service :basetools)
  (:export :start-holiday-managing-server :start-holiday-managing-server-script))

(in-package holiday-manager)

(defconstant field-separator #\|
  "Field separator used in all serialised files. This is only necessary for the holiday files.
The server communication would provide its own separator.")

(defconstant usr-extension "usr"
  "Filename extension used for user holiday files")

;; Helper functions, not exported

(defun add-user-holiday (user-hash user start days &key work-dir (file-sync T))
  "Add a new holiday for a user. Sync file when ready or rollback on error if file-sync is T."
					; Insert part from read-single-user-config here to insert a single holiday
  (if (or (>= 0 days) (>= start 10000000)) (error 'socket-function-error :msg "Invalid start date or duration"))
  (let (previous next user-htable sync-args
		 (user-conf (gethash user user-hash))
		 (cur (list (cons start days)))
		 (end (+ start days)))
    (if (eq user-conf NIL) (error "Add user first before adding holidays"))
    (setf user-htable (gethash "htable" user-conf))
    (setf previous
	  (find-previous user-htable (cons start days) :gt-than (lambda (f s) (> (car f) (car s)))))
    (cond ((eq previous NIL)            ; No previous, but possibly there is a next entry
	   (unless (eq user-htable NIL) (setf next user-htable)))
	  (T
	   (setf next (cdr previous))))
    (when next
      (cond ((< (car next) end) (error "New/changed holiday overlaps following"))
	    (T (setf (cdr cur) next)))) ; chain current holiday with next holiday in list
    (cond (previous
	   (cond ((eql (car (first previous)) start) ; special case: just replace data of previous, no new chaining.
					; Compare with next not necessary as we have already done that above
		  (setf (first previous) (cons start days)))
		 (T                     ; The previous entry really is a previous entry
		  (cond ((> (+ (car (first previous)) (cdr (first previous)))) (error "Previous entry overlaps new entry"))
			(T (setf (cdr previous) cur)))))) ; chain previous to new entry
	  (T                            ; No previous, set current as new start of the list
	   (setf (gethash "htable" user-conf) cur)))
    (format T "Synchronize user holiday...~%")
    (when file-sync
      (setf sync-args (list
		       (format NIL "~A.~A" user usr-extension) ;1st argument: file name
					; 2nd argument: list containing all output
					; Use a cons cell with fixed 1st argument and the generated list as 2nd
		       (cons (format NIL "~A" (gethash "group" user-conf))
			     (map 'list
				  (lambda (h) (format NIL "~A~C~A" (car h) field-separator (cdr h)))
				  (gethash "htable" user-conf)))))
      (if work-dir (setf (cdr (cdr sync-args)) (list :directory work-dir)))
      (apply #'sync-to-file sync-args))))

(defun del-user-holiday (user-hash user start &key work-dir (file-sync T))
  "Delete a particular holiday from user-hash for user with begin start"
  (let ((user-conf (gethash user user-hash)) cur previous user-htable sync-args)
    (if (eq user-conf nil) (error "User does not exist"))
    (setf user-htable (gethash "htable" user-conf))
    (if (eq user-htable NIL) (error "No such holiday for this user"))
    (cond ((eq (car (first user-htable)) start)
					; Holiday is the first in the list
	   (setf (gethash "htable" user-conf) (cdr user-htable))) ; list now begins with its cdr
	  (T ; The list is not empty -> if previous found, rechain previous with next
					; Not found means our entry is the first, which it is not -> error
	   (setf previous
		 (find-previous user-htable (cons (1- start) 1) :gt-than (lambda (f s) (< (car f) (car s)))))
	   (setf cur (cdr previous))
	   (unless (and cur (eql (car (first cur)) start)) (error "No such holiday for this user"))
	   (setf (cdr previous) (cdr cur))))
    (when file-sync
      (setf sync-args (list
		       (format NIL "~A.~A" user usr-extension)
		       (cons (format NIL "~A" (gethash "group" user-conf))
			     (map 'list
				  (lambda (h) (format NIL "~A~C~A" (car h) field-separator (cdr h)))
				  (gethash "htable" user-conf)))))
      (if work-dir (setf (cdr (cdr sync-args)) (list :directory work-dir)))
      (apply #'sync-to-file sync-args))))


(defun read-single-user-config (u-file)
  "Read in one user's holdidays from file. Format is a list of cons cells containing start date and duration."
  (let ((result (make-hash-table :test #'equal)) (htable (cons NIL NIL)) pos line)
    (setf pos htable)
    (with-open-file (fd u-file) 
      (setf line (read-line fd))
      (setf line (string-trim '(#\Return) line))
      (setf (gethash "group" result) line)
      (do  ((line (read-line fd NIL) (read-line fd NIL)) start days) ((eq line NIL))
	(setf line (split-string line field-separator))
	(setf start (parse-integer (car line)))
	(setf days (parse-integer (second line)))
	(cond ((eq (car pos) NIL) (setf (car pos) (cons start days))) ; add 1st holiday
	      ((>= (+ (car (car pos)) (cdr (car pos))) start) (error "User file error - overlapping or unsorted holidays"))
	      (T (setf (cdr pos) (cons (cons start days) NIL)) (setf pos (cdr pos))))))
    (setf (gethash "htable" result) (if (eq (car pos) NIL) NIL htable))
    result))



(defun read-user-configs (user-path)
  "Read in all user holidays from all files"
  (let ((result-hash (make-hash-table :test #'equal)))
					; Now read in the files one by one, add them to the global hash and return it
    (dolist (u-file (directory (make-pathname :name :wild :type usr-extension :defaults user-path)) result-hash)
      (setf (gethash (pathname-name u-file) result-hash) (read-single-user-config u-file)))))


(defun read-global-config (path file)
  "Read global configuration and return corresponding structure"
  (let ((conf (read-config-file (format NIL "~A~A" path file))))
    (setf conf (gethash "global" conf))
    (unless conf (error "Global section not found in configuration file ~A~%" file))
    (first conf)))

;;Network functions exported via socket interface, not lisp-exported

(defun get-users (argv outfd mem &key &allow-other-keys)
  "Get users currently stored."
  (declare (ignore argv))
  (maphash (lambda (key _) (declare (ignore _)) (funcall outfd (list key))) (getattr mem "user-config"))
  (funcall outfd))


(defun add-holiday (argv outfd mem &key &allow-other-keys)
  "Add one holiday to users holiday table. Function matching socket-server interface"
  (add-user-holiday
   (getattr mem "user-config") (first argv) (parse-integer (second argv)) (parse-integer (third argv))
   :work-dir (getattr mem "work-dir"))
  (funcall outfd))


(defun delete-holiday (argv outfd mem &key &allow-other-keys)
  "Delete one holiday from users holiday table. Function matching socket-server interface"
  (del-user-holiday
   (getattr mem "user-config") (first argv) (parse-integer (second argv)) :work-dir (getattr mem "work-dir"))
  (funcall outfd))


(defun get-user-holidays (argv outfd mem &key &allow-other-keys)
  "Get holiday table for one user. Function matching socket-server interface"
  (dolist (v (gethash "htable" (gethash (first argv) (getattr mem "user-config"))))
    (funcall outfd (list (princ-to-string (car v)) (princ-to-string (cdr v)))))
  (funcall outfd))

(defun get-all-holidays (argv outfd mem &key &allow-other-keys)
  "Get holiday tables for all users. Function matching socket-server interface"
  (declare (ignore argv))
  (maphash
   (lambda (k v)
     (dolist (h (gethash "htable" v))
       (funcall outfd (list k (princ-to-string (car h)) (princ-to-string (cdr h))))))
   (getattr mem "user-config"))
  (funcall outfd))


(defun add-user (argv outfd mem &key priv &allow-other-keys)
  "Add new user to holiday users."
  (unless priv (error "Create user only works over a privileged connection"))
  (let ((user-hash (getattr mem "user-config" :existing-flag T))
	(user (first argv))
	(user-conf (make-hash-table :test #'equal)))
    (if (gethash user user-hash) (error "User already exists -- cannot create"))
    (setf (gethash "htable" user-conf) NIL)
    (setf (gethash "group" user-conf) (second argv))
    (setf (gethash user user-hash) user-conf)
    (sync-to-file (format NIL "~A.~A" user usr-extension) (list (second argv)) :directory (getattr mem "work-dir")))
  (funcall outfd))


(defun delete-user (argv outfd mem &key priv &allow-other-keys)
  "Delete user from holiday users."
  (unless priv (error "Delete user only works over a privileged connection"))
  (let ((user-hash (getattr mem "user-config" :existing-flag T))
	(user (first argv)))
    (unless (gethash user user-hash) (error "User does not exist - cannot delete"))
    (remhash user user-hash)
    (delete-file (make-pathname :name user :type usr-extension :defaults (getattr mem "work-dir"))))
  (funcall outfd))


(defun sync (argv outfd mem &key priv &allow-other-keys)
  "Synchronise shared memory to disk.
Currently everything is synchronized on write so there is nothing to do here."
  (declare (ignore argv mem priv))
  (funcall outfd))



;; Main function doing everything. Exported!
(defun start-holiday-managing-server (&key (global-config "holiday.conf") (config-dir "./") (work-dir "./"))
  "Read in global configuration file, read in all user holidays and start service."
  (format T "Parameter init...~%")
  (if (not (ends-with work-dir #\/)) (error "No pathname for work-dir"))
  (if (not (ends-with config-dir #\/)) (error "No pathname for config-dir"))
  (let (a-config port addr priv logf sock (attr (make-instance 'attribute-collection)))
					; read configuration files
    (setf a-config (read-global-config config-dir global-config))
					; dump config
					; (maphash #'(lambda (k v) (format T "K:~A,V:~A~%" k v)) a-config)
    (unless (and (gethash "port" a-config) (gethash "name" a-config)) (error "name or port not set in config"))
    (setf port (first (gethash "port" a-config)))
    (setf addr (first (gethash "name" a-config)))
    (if (gethash "privileged" a-config) (setf priv (gethash "privileged" a-config)))
    (if (gethash "logfile" a-config) (setf logf (format NIL "~A~A" work-dir (gethash "logfile" a-config))))
    (setf a-config (read-user-configs work-dir))
					; Create attribute-collection from configurations
    (setattr attr "user-config" a-config)
    (setattr attr "work-dir" work-dir)
					; Now instantiate socket-server for remote connections
    (format T "Create socket-server instance at address ~A:~A...~%" addr port)
    (setf sock (make-instance 'socket-server :bindaddr addr :port port :init-shared-mem attr :privileged-addresses priv))
					; Add all service functions
    (format T "Register network functions...")
    (push-handler sock "getl" #'get-users) ; Not in Perl prototype.
    (push-handler sock "addh" #'add-holiday)
    (push-handler sock "delh" #'delete-holiday)
    (push-handler sock "getu" #'get-user-holidays)
    (push-handler sock "geta" #'get-all-holidays)
    (push-handler sock "addu" #'add-user)
    (push-handler sock "delu" #'delete-user) ;Not in Perl prototype.
    (push-handler sock "sync" #'sync) ;Not in Perl prototype. Possibly useful, but does nothing currently.
					; Start run-services-until-shutdown for servicing
    (format T "Start service!~%")
    (run-services-until-shutdown sock)))


;; Wrapper for main function to call it from the command line. Exported!
;; This shows how to build a command line containing keyword arguments for a later call.
(defun start-holiday-managing-server-script ()
  "Start holiday managing server. Parse arguments given on the command line and call main function then..."
  (let ((callv (cons NIL NIL)))
    (do ((argv-it (cdr sb-ext:*posix-argv*)) (callv-it callv)) ((eq argv-it NIL))
      (cond
	((equal (first argv-it) "-w")
	 (setf (cdr callv-it) (list :work-dir (second argv-it))) ;append to existing list, advance both lists twice
	 (setf argv-it (nthcdr 1 argv-it))
	 (setf callv-it (nthcdr 1 callv-it)))
	((equal (first argv-it) "-f")
	 (setf (cdr callv-it) (list :global-config (second argv-it)))
	 (setf argv-it (nthcdr 1 argv-it))
	 (setf callv-it (nthcdr 1 callv-it)))
	((equal (first argv-it) "-c")
	 (setf (cdr callv-it) (list :config-dir (second argv-it)))
	 (setf argv-it (nthcdr 1 argv-it))
	 (setf callv-it (nthcdr 1 callv-it)))))
    (format T "Call holiday starter. Parsed command line arguments: ~A~%" (cdr callv))
    (apply #'start-holiday-managing-server (cdr callv))))
