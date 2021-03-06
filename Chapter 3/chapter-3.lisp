(in-package :chapter-3-package)

(defun make-cd (title artist rating ripped)
  "Instantiates CDs. Returns a p-list that represents the CD."
  (list :title title :artist artist :rating rating :ripped ripped))

;; Creating the database as a global variable:
(defvar *db* nil)

(defun add-record (cd)
  "Adds CDs to the database. Returns the updated database."
  (push cd *db*)) ; 'push' adds a given element to a given list.

(defun dump-db ()
  "Pretty-prints the database. Returns NIL."
  (dolist (cd *db*) ; iterating through the list
    ;; pretty-printing each element in list:
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  "Asks the user for information given a prompt. Returns the user input."
  (format *query-io* "~a: " prompt) ; printing the given prompt followed by ': '
  (force-output *query-io*) ; ensuring that the stream won't wait for a newline character to print
  (read-line *query-io*))

(defun prompt-for-cd ()
  "Uses prompt-read to register a CD. Returns a CD object."
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p (prompt-read "Ripped [y/n]"))))

(defun add-cds ()
  "Loops as long as the user wants to registering new CDs. Returns NIL."
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  "Saves the database given a filename. Returns NIL."
  (with-open-file (out filename
		       :direction :output ; Indicating 'write mode'
		       :if-exists :supersede) ; Overwriting a file in case it already exists.
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  "Loads the database into the program given its name. Returns the database."
  (with-open-file (in filename)
    (with-standard-io-syntax ; Ensuring the file will be read the same way it was written.
      (setf *db* (read in))))) ; Changing the *db* variable's value.

(defun select (selector-fn)
  "Prints the CDs found given a selector function. Returns a list of CD objects."
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  "Updates CDs given a selector function. Returns the updated database."
  (setf *db* ; Changing *db*'s value:
	(mapcar
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title    (setf (getf row :title)  title))  ; Checking whether title was passed as argument or not.
	       (if artist   (setf (getf row :artist) artist))
	       (if rating   (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))

(defun delete-rows (selector-fn)
  "Deletes CDs given a selector function. Returns the updated database."
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  "Generates an expression that compares a given CD object field to a given value. Returns the expression."
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields) ; (fields) -> (:artist "Beatles" :ripped t ...)
  "Iterates through a list of fields and values (alternated) and applies 'make-comparison-expr' to each pair. Returns all the expressions unevaluated."
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields)))) ; 'pop' removes the first element in the list and returns its value.

(defmacro where (&rest clauses)
  "Generates code to compare only the specified fields. Generates a list of comparison expressions inside an AND operator."
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses)))) ; ,@ incorporates the elements of the following list to the outer list.

