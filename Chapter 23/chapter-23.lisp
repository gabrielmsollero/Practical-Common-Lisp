;; Comandos p/ usar no REPL antes de carregar
;; (load "~/quicklisp/setup.lisp")
;; (ql:quickload :cl-ppcre)
;; (load "pathnames.lisp")

(defpackage :com.gigamonkeys.spam
  (:use :common-lisp :com.gigamonkeys.pathnames :cl-ppcre))

(in-package :com.gigamonkeys.spam)

(defclass word-feature ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply :word")
    :documentation "The word this feature represents.")
   (spam-count
    :initarg :spam-count
    :accessor spam-count
    :initform 0
    :documentation "Number of spams we have seen this feature in.")
   (ham-count
    :initarg :ham-count
    :accessor ham-count
    :initform 0
    :documentation "Number of hams we have seen this feature in.")))
    
(defparameter *max-chars* (* 10 1024))
(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))
(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)
(defvar *total-hams* 0)
(defvar *total-spams* 0)
(defvar *feature-database* (make-hash-table :test #'equal))


(defun clear-database ()
  "Removes all the information contained in *feature-database*."
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-spams* 0
   *total-hams* 0))

(defun classify (text)
  "Main function. Returns the classification directly from the text."
  (classification (score (extract-features text))))

(defun classification (score)
  "Returns a score and its classification."
  (values
   (cond
     ((<= score *max-ham-score*) 'ham)
     ((>= score *min-spam-score*) 'spam)
     (t 'unsure))
   score))

(defun intern-feature (word)
  "Either creates or finds a feature connected to word, and then returns it."
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
	    (make-instance 'word-feature :word word))))

(defun extract-words (text)
  "Splits the text into words."
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  "Interns all the words in text and returns a feature for each one of them."
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  "Defines the way word-features should be printed."
  (print-unreadable-object (object stream :type t)
    (with-slots (word ham-count spam-count) object
      (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(defun train (text type)
  "Trains the filter based on a text and its label."
  (dolist (feature (extract-features text))
    (increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  "Increases the count of spam or ham of a feature (word)."
  (ecase type
    (ham (incf (ham-count feature)))
    (spam (incf (spam-count feature)))))

(defun increment-total-count (type)
  "Increases the total count of ham or spam seen."
  (ecase type
    (ham (incf *total-hams*))
    (spam (incf *total-spams*))))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
	  (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional
					    (assumed-probability .5)
					    (weight 1))
  "Calculates the bayesian probability of a given feature be in a spam."
  (let ((basic-probability (spam-probability feature))
	(data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
	  (* data-points basic-probability))
       (+ weight data-points))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
      for i below (/ degrees-of-freedom 2)
      for prob = (exp (- m)) then (* prob (/ m i))
      summing prob)
   1.0))

(defun fisher (probs number-of-probs)
  "The fisher computation described by Robinson."
  (inverse-chi-square
   (* -2 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun score (features)
  "Combines the probability of a list of features compose a spam and a ham into a single probability of spam."
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
	(let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
	  (push spam-prob spam-probs)
	  (push (- 1.0d0 spam-prob) ham-probs)
	  (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
	  (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun add-file-to-corpus (filename type corpus)
  "Adds a labeled file to the corpus."
  (vector-push-extend (list filename type) corpus))

(defun add-directory-to-corpus (dir type corpus)
  "Adds each file in directory to the corpus, labeled as type."
  (dolist (filename (list-directory dir))
    (add-file-to-corpus filename type corpus)))

(defun test-classifier (corpus testing-fraction)
  "Trains a classifier on part of a corpus and then tests it on the rest of the corpus."
  (clear-database)
  (let* ((shuffled (shuffle-vector corpus))
	 (size (length corpus))
	 (train-on (floor (* size (- 1 testing-fraction)))))
    (train-from-corpus shuffled :start 0 :end train-on)
    (test-from-corpus shuffled :start train-on)))

(defun train-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) do
       (destructuring-bind (file type) (aref corpus idx)
	 (train (start-of-file file *max-chars*) type))))

(defun test-from-corpus (corpus &key (start 0) end)
  (loop for idx from start below (or end (length corpus)) collect
       (destructuring-bind (file type) (aref corpus idx)
	 (multiple-value-bind (classification score)
	     (classify (start-of-file file *max-chars*))
	 (list
	  :file file
	  :type type
	  :classification classification
	  :score score)))))

(defun nshuffle-vector (vector)
  (loop for idx downfrom (1- (length vector)) to 1
     for other = (random (1+ idx))
     do (unless (= idx other)
	  (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (nshuffle-vector (copy-seq vector)))

(defun start-of-file (file max-chars)
  (with-open-file (in file)
    (let* ((length (min (file-length in) max-chars))
	   (text (make-string length))
	   (read (read-sequence text in)))
      (if (< read length)
	  (subseq text 0 read)
	  text))))

(defun result-type (result)
  (destructuring-bind (&key type classification &allow-other-keys) result
    (ecase type
      (ham
       (ecase classification
	 (ham 'correct)
	 (spam 'false-positive)
	 (unsere 'missed-ham)))
      (spam
       (ecase classification
	 (spam 'correct)
	 (ham 'false-negative)
	 (unsere 'missed-spam))))))

(defun false-positive-p (result)
  (eql (result-type result) 'false-positive))

(defun false-negative-p (result)
  (eql (result-type result) 'false-negative))

(defun missed-ham-p (result)
  (eql (result-type result) 'missed-ham))

(defun missed-spam-p (result)
  (eql (result-type result) 'missed-spam))

(defun correct-p (result)
  (eql (result-type result) 'correct))

(defun analyze-results (results)
  (let* ((keys '(total correct false-positive false-negative missed-ham missed-spam))
	 (counts (loop for x in keys collect (cons x 0))))
    (dolist (item results)
      (incf (cdr (assoc 'total counts)))
      (incf (cdr (assoc (result-type item) counts))))
    (loop with total = (cdr (assoc 'total counts))
       for (label . count) in counts
       do (format t "~&~@(~a~):~20t~5d~,5t: ~6,2f%~%"
		  label count (* 100 (/ count total))))))

(defun show-summary (file text classification score)
  (format t "~&~a" file)
  (format t "~2%~a~2%" text)
  (format t "Classified as ~a with score of ~,5f%" classification score))

(defun show-feature (feature)
  (with-slots (word ham-count spam-count) feature
    (format
     t "~&~2t~a~30thams: ~5d; spams: ~5d;~,10tprob: ~,f~%"
     word ham-count spam-count (bayesian-spam-probability feature))))

(defun sorted-interesting (features)
  (sort (remove-if #'untrained-p features) #'< :key #'bayesian-spam-probability))
