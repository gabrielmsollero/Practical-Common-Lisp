;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chapter-15-asd (:use :cl :asdf) (:export :chapter-15-system))
(in-package :chapter-15-asd)

(defsystem chapter-15-system
    :name "chapter-15-system"
    :components
    ((:file "chapter-15-package")
     (:file "chapter-15" :depends-on ("chapter-15-package"))))
