;;;; So I was into Practical Common Lisp and decided to properly create system and package files for the chapters I'd done only after chapter 23.
;;;; I made an exercise for myself out of it and coded a function for creating all the repetitive code for system, package and code files (system
;;;; declaration, package creation, etc.) With this, I can call make-chapter-folder with a number and the chapter folder in a predefined path is
;;;; created. I'm sorry if there is any bad practice, I'm new to lisp and for now I only know what was passed in Practical Common Lisp until chapter 24.

(in-package :make-chapter-folder-package)

(defconstant +path+ (pathname-as-directory "~/Desktop/Lisp Hackers/")) ; This is where you put your chapters folder.

;; with-open-chapter-file : stream string keywords -> boolean. Returns true only when all the files are created correctly.
(defmacro with-open-chapter-file ((streamvar component
					     &key
					     (directory (sb-posix:getcwd)) (type "lisp")
					     (chapter-number nil) (if-exists :error))
				  &body body) ; I tried to make the structure look like with-open-file, but there
					      ; is a string and a list of keywords where the path should be.
  "A version of with-open-file with convenient tools for creating chapter files, which are repetitive. Returns true when all the files are created."
  `(with-open-file (,streamvar (make-pathname ; creating the file in which the text will be written.
				:name ,(if chapter-number
					  `(format nil "chapter-~a~:[~;-~:*~a~]" ,chapter-number ,component)
					  component)
			       :type ,type
			       :defaults ,directory)
			       :direction :output
			       :if-exists ,if-exists)
    ,@body))

;; make-chapter-folder : number keyword -> boolean
(defun make-chapter-folder (n &key export if-exists)
  "Creates system, package and code files for a chapter in the book."
  (let* ((n (write-to-string n))
	 (chapter-path ; this is the predefined path I talked about, and can be changed in the parameter +path+.
	  (pathname-as-directory (make-pathname :name (concatenate 'string "Chapter " n)
						:defaults +path+))))
    (sb-posix:chdir (ensure-directories-exist chapter-path))
    (and (make-chapter-system :path chapter-path :chapter-number n :if-exists if-exists)
	 (make-chapter-package :path chapter-path :chapter-number n :export export :if-exists if-exists)
	 (make-chapter-file :path chapter-path :chapter-number n :if-exists if-exists))))


;; make-chapter-system : string number keyword -> boolean
(defun make-chapter-system (&key path chapter-number (if-exists :error))
  "Creates system file for a chapter, declaring the standard dependencies."
  (with-open-chapter-file (out "system" :directory path :type "asd" :chapter-number chapter-number :if-exists if-exists)
    (format
     out ; the next string is the template for a .asd file for one chapter, filled repeatedly with n.
     ";;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chapter-~a~:*-asd (:use :cl :asdf) (:export :chapter-~a~:*-system))
(in-package :chapter-~a~:*-asd)

(defsystem chapter-~a~:*-system
    :name 'chapter-~a~:*-system
    :components
    ((:file 'chapter-~a~:*-package)
     (:file 'chapter-~a~:* :depends-on ('chapter-~a-package))))" chapter-number)
t))

;; make-chapter-package : string number list keyword -> boolean
(defun make-chapter-package (&key path chapter-number export (if-exists :error))
  "Creates package file for a chapter, possibly exporting some symbols."
  (with-open-chapter-file (out "package" :directory path :chapter-number chapter-number :if-exists if-exists)
    (format
     out ; template for a .lisp file in which a chapter package is declared.
     "(in-package :cl-user)

(defpackage :chapter-~a-package
  (:use :common-lisp)~:[~;~%~:*  (:export~{~%   :~a~})~])" chapter-number (mapcar #'string-downcase export))
  t))

;; make-chapter-file : string number keyword -> boolean
(defun make-chapter-file (&key path chapter-number (if-exists :error))
  "Creates the code file for a chapter, declaring the package in which it will be made."
  (with-open-chapter-file (out nil :directory path :chapter-number chapter-number :if-exists if-exists)
    (format out "(in-package :chapter-~a-package)~%" chapter-number) ; there is not much to write in the code file, but I inserted this standard first line anyways.
    t))
