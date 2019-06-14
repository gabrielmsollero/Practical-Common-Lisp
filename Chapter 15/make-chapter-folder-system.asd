;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:make-chapter-folder-asd (:use :common-lisp :asdf) (:export :make-chapter-folder-system))
(in-package :make-chapter-folder-asd)

(defsystem make-chapter-folder-system
    :name "make-chapter-folder-system"
    :depends-on ("chapter-15-system")
    :components
    ((:file "make-chapter-folder-package")
     (:file "make-chapter-folder" :depends-on ("make-chapter-folder-package"))))
