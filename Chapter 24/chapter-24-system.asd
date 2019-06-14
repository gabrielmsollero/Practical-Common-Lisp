;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage #:chapter-24-asd
  (:use :cl :asdf))

(in-package :chapter-24-asd)

(defsystem chapter-24
    :name "chapter-24"
    :components ((:file "package-24")
		 (:file "chapter-24" :depends-on ("package-24")))
    :depends-on ("macro-utilities"))
