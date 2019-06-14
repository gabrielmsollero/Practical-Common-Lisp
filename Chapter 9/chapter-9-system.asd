;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:chapter-9-asd (:use :cl :asdf) (:export :chapter-9-system))
(in-package :chapter-9-asd)

(defsystem chapter-9-system
    :name 'chapter-9-system
    :components
    ((:file 'chapter-9-package)
     (:file 'chapter-9 :depends-on ('chapter-9-package))))