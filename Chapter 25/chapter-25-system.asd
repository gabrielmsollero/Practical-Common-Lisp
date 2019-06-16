;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chapter-25-asd (:use :cl :asdf) (:export :chapter-25-system))
(in-package :chapter-25-asd)

(defsystem chapter-25-system
  :name 'chapter-25-system
  :components
  ((:file "chapter-25-package")
   (:file "chapter-25" :depends-on ("chapter-25-package")))
  :depends-on ("chapter-24-system" "chapter-15-system"))
