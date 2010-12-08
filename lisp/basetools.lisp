;;; Author: Thorsten Rangwich
;;; See file LICENSE for copyright and usage of this code.
;;;
;;; Tools package, some base tools used everywhere are defined here
;; ToDo:
;; Add logger
;; add printer method for attribute-collection and / or dictionary

;; Package definition

(defpackage :basetools
  (:use :cl)
  (:export 
     :match-chars :split-string :ends-with ; simple string manipulation
     :find-previous ; list helper functions
     :read-config-file ; config-file-reader
     :sync-to-file ;save list of lines to text file, do secure write and rename afterwards
     :attribute-collection :hasattr :getattr :setattr ; attribute-collection class with methods, no slot names exported
     ))

(in-package :basetools)

;; String functions

(defun match-chars (the-string &rest matchlist)
  "Take a string and match it agains a range of characters. If one character does not match, return NIL, else return T.
Ranges are given in matchlist, every match may be a character or a list of two characters marking the range,
e.g. #\. '(#\0 #\9) for dot and 0-9."
  (check-type the-string string)
  (map 
   nil 
   (lambda (ch)
     (let ((chcode (char-code ch)))
					; iterate over match list, as soon as we have a match, return
					; if whole loop does not find a match, return result NIL
       (dolist (c matchlist (return-from match-chars NIL))
	 (cond
	   ((characterp c) (if (eql c ch) (return)))
	   ((listp c) 
	    (if (not (eql (length c) 2)) (error "Only characters or ranges for match-chars"))
	    (if (and (>= chcode (char-code (car c))) (<= chcode (char-code (nth 1 c)))) (return)))
	   (T (error "Only character ranges and characters allowed"))))))
   the-string)
					; All characters in string got match
  T)

(defun ends-with (str ch)
  "Test if string ends with given character"
  (progn
    (unless (and (stringp str) (characterp ch)) (error "ends-with expects arguments string and character"))
    (eq ch (char str (1- (length str))))))

(defun split-string (str token)
  "Split string at character token, return list of strings."
  (cond
   ((and (stringp str) (characterp token)))
   (T (error "split-string: expected string to split at character")))
  (loop
   with res = (cons (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t) NIL)
   with ptr = res
   for i across str
   do
   (cond
    ((eql i token) (setf (cdr ptr) (cons (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t) nil))
     (setf ptr (cdr ptr)))
    (T (vector-push-extend i (car ptr))))
   finally (return res)))

;; dictionary extensions

(defmethod setdefault ((ht hash-table) key val)
  "Traverse dictionaries. Return inner dictionary value, create, if not there.
TODO: This is far away from serving Python's setdefault() use. Macro needed? Use (or), then
the value need not to be written down again?"
  (multiple-value-bind (_ ind) (gethash key ht) (declare (ignore _)) (if ind (gethash key ht) (setf (gethash key ht) val))))

;; List helpers
(defun find-previous (search-list start &key (gt-than #'>))
  "Iterate over a list. Find the first entry before 'start'. Returns the cons cell of it.
If no start is less or equal to the first entry, NIL is returned.
If gt-than is given, entries are compared using this function"
  (cond ((eq search-list NIL) NIL)
	((funcall gt-than (car search-list) start) NIL)
	(T
	 (do ((prev search-list (cdr prev))) ((or (eq (cdr prev) NIL) (funcall gt-than (car (cdr prev)) start)) prev)))))

;; File functions

;; synchronised file write
(defun sync-to-file (file input &key (directory NIL dirp))
  "Write contents of variable input to a file.
Input may be a list or a stream. File may be pathname or string. If given, directory may be pathname or string.
directory may be a string or a pathname. In the latter case drive and path are taken from the pathname object.
Support for typical lisp directory style using cons is not implemented, because in that case it would be
necessary to implement a drive letter option for windows as well."
					; FIXME: handle files without extension better
  (let (getline fd-in path-out path-out-tmp)
					; Create function to read one line from input
    (cond (
	   (listp input)
	   (setf fd-in input)
	   (setf getline
		 (lambda ()
		   (let (r)
		     (when (car fd-in)
		       (setf r (car fd-in))
		       (setf fd-in (cdr fd-in))
		       (format NIL "~A~%" r))))))
	  (
	   (streamp input)
	   (setf getline (lambda () (let ((l (read-line input NIL))) (unless (eq l NIL) (format NIL "~A~%" l))))))
	  (T (error "Input must be file descriptor or list")))
					; Create pathname objects for final and temporary output name
    (cond ((stringp file) (setf path-out (pathname file)))
	  ((pathnamep file))
	  (T (error "file must be either string or pathname")))
    (when dirp
      (cond ((stringp directory)
	     (unless (ends-with directory #\/) (error "Directory name not terminated"))
					; Ugly: SBCL treats :directory "xyz" always as absolute. merge-pathnames fixes that.
	     (setf path-out (make-pathname :defaults path-out :directory (pathname-directory (merge-pathnames directory)))))
	    ((pathnamep directory)
	     (setf path-out (make-pathname :defaults path-out :directory (pathname-directory directory))))
	    (T (error "directory must be string or pathname"))))
    (setf path-out-tmp (make-pathname :defaults path-out :type (concatenate 'string (pathname-type path-out) ".tmp")))
					; If stream input, eventually open output with the same parameters as input
    (with-open-file (out-fd path-out-tmp :direction :output :if-exists :supersede)
      (do ((line (funcall getline) (funcall getline))) ((eq line NIL))
	(write-sequence line out-fd)))
					; This may fail on windows if target exists - delete the target eventually
    (handler-case (rename-file path-out-tmp path-out)
      (file-already-exists () ; If the underlying operating system cannot handle move to existing file -- e.g. Windows
	(delete-file path-out)
	(rename-file path-out-tmp path-out)))
    T))

;; config file reader

(define-condition config-file-parser-error (error)
  ((text :initarg :line :reader line)))

(defun read-config-file (cfgfile &key)

  "Read configuration file (currently java properties file) and return a configuration hashtable.
FIXME: Use exception handling to ignore corrupt lines.
FIXME: Append is not very effectively, solve this as in append recipe given
FIXME: This still cannot handle these inline dictionaries
FIXME: Use with-open-file instead of open file handle"

  (let ((value-hash (make-hash-table :test #'equal))
	(eol-hash (make-hash-table :test #'equal))
	pos key val (conf-fd (open cfgfile)) sec)
    (declare (ignore eol-hash)) ; Not yet needed
    (setf sec value-hash)
    (loop for line = (read-line conf-fd nil)
	  while line do
	  ;(format t "Line read:~A.~%" line)
	  (setf line (string-trim '(#\Space #\Tab #\Return) line))
	  (when (and (> (length line) 0) (not (eql (char line 0) #\#)))
	    (cond ((eql (char line 0) #\[)
		   (if (not (eql (char line (1- (length line))) #\])) (error "Invalid line:~A" line))
		   (setf sec (let* 
				 ((secname (subseq line 1 (1- (length line)))) 
				  (sec (gethash secname value-hash))
				  (new-ht (make-hash-table :test #'equal)))
			       ;(format t "change to section .~A.~%" secname)
			       (if sec (nconc (gethash secname value-hash) (list new-ht))
				 (setf (gethash secname value-hash) (list new-ht)))
			       new-ht))
		   ;(format t "sec:~A~%" sec)
		   ;(format t "value-hash:~A" value-hash)
		   )
		  (T
		   (setf pos (position #\= line))
		   (when (and pos (> pos 0))
		     (setf key (subseq line 0 pos))
		     (setf val (subseq line (1+ pos)))
		     ;(format t "Set key/val .~A.~A.~%" key val)
		     (cond ((and
			     (or (eql (char val 0) #\") (eql (char val 0) #\'))
			     (eql (char val 0) (char val (1- (length val)))))
			    (setf val (subseq val 1 (1- (length val)))))
			   ((eql 0 (length val)) (setf val nil))
			   ((not (find #\. val)) (setf val (parse-integer val)))
			   ((find #\. val) ; this looks like a float. Check characters and parse it
			    (if (match-chars val '(#\0 #\9) #\. #\+ #\- #\e)
				(setf val (read-from-string val))
				(error (format nil "Invalid float, line:~A." line))))
			   (T (error (format nil "Invalid format, line:~A." line))))
		     (setf pos (gethash key sec))
		     (if pos (nconc pos (list val)) (setf (gethash key sec) (list val))))))))
    (close conf-fd)
    value-hash))

;; simple class to hold attributes, dictionary extension in some way

(defgeneric getattr (attrs key &key existing-flag))
(defgeneric hasattr (attrs key))
(defgeneric setattr (attrs key T &key existing-flag))


(defclass attribute-collection ()
  ((storage :initform (make-hash-table :test #'equal) :reader smem
	    :documentation "Attribute dictionary, change contents, but do not exchange it completely!")
   (init-from :initform NIL :initarg :init-from :documentation "Initialise from attribute collection or hastable"))
  (:documentation "Attribute collection, similar to dictionary with some accessors"))

(defmethod initialize-instance :after ((coll attribute-collection) &key (create-copy NIL))
  "Check init-from for correct type, initialise storage"
  (let ((copy-source NIL))
    (with-slots (storage init-from) coll
      (if (not (eq init-from NIL))

	(cond
	  ((hash-table-p init-from) (setf copy-source init-from))
	  ((typep init-from 'attribute-collection) (setf copy-source (slot-value init-from 'storage)))
	  (T (error "Attribute collection content initialised from invalid type"))))
      (if (not (eq copy-source NIL))
	  (if create-copy
	      (error "Create attribute-collection as copy is not yet implemented")
	      (setf storage copy-source))))))

(defmethod getattr ((attrs attribute-collection) (key string) &key existing-flag)
  "Get attribute from shared class mem. If existing flag is set and different from zero, the attribute must exist"
  (multiple-value-bind (res ind) (gethash key (smem attrs))
		       (cond
			(ind res)
			((eq NIL existing-flag) res)
			((eql existing-flag 0) res)
			(T (error "Value not set")))))

(defmethod hasattr ((attrs attribute-collection) (key string))
  "Query if given attribute does exist"
  (multiple-value-bind (_ is-set) (gethash key (smem attrs)) (declare (ignore _))
		       is-set))

(defmethod setattr ((attrs attribute-collection) (key string) (val T) &key (existing-flag NIL existing-flag-p))
  "Set attribute. If existing flag is given, depending on existing-flag the key must exist or must not exist."
  (cond 
   ((eql existing-flag-p NIL) (setf (gethash key (smem attrs)) val))
   ((eql existing-flag NIL) (multiple-value-bind (_ ind) (gethash key (smem attrs)) (declare (ignore _))
				       (if (eql ind NIL) (setf (gethash key (smem attrs)) val) (error "Already exists"))))
   (T (multiple-value-bind (_ ind) (gethash key (smem attrs)) (declare (ignore _))
			   (if (eql ind NIL) (error "Required key does not exist for setattr") (setf (gethash key (smem attrs)) val))))))


;;; end package basetools
