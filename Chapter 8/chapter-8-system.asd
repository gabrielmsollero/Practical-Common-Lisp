;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chapter-8-asd (:use :cl :asdf))
(in-package :chapter-8-asd)

(defsystem chapter-8-system
  :name "chapter-8-system"
  :components
  ((:file "chapter-8-package")
   (:file "chapter-8" :depends-on ("chapter-8-package"))))
