(in-package :cl-user)

(defpackage :chapter-25-package
  (:use :common-lisp
	:chapter-24-package
	:chapter-15-package)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))
