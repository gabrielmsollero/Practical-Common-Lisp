;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chapter-3-asd (:use :cl :asdf))
(in-package :chapter-3-asd)

(defsystem chapter-3-system
    :name "chapter-3-system"
    :components
    ((:file "chapter-3-package")
     (:file "chapter-3" :depends-on ("chapter-3-package"))))
