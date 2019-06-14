;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chapter-23-asd (:use :cl :asdf) (:export :chapter-23-system))
(in-package :chapter-23-asd)

(defsystem chapter-23-system
    :name 'chapter-23-system
    :components
    ((:file 'chapter-23-package)
     (:file 'chapter-23 :depends-on ('chapter-23-package))))