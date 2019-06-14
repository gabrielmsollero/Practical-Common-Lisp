(in-package :cl-user)

(defpackage :make-chapter-folder-package
  (:use :cl :chapter-15-package)
  (:export
   :make-chapter-folder
   :make-chapter-system
   :make-chapter-package
   :make-chapter-file
   :with-gensyms
   :with-open-chapter-file))
