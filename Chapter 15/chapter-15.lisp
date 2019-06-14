(in-package :chapter-15-package)

(defun component-present-p (value)
  "Verifies if a pathname component is present."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  "Verifies if a given path is a directory name. If it doesn't have name and tye, then it is a directory."
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  "Converts a pathname to directory form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname) ; Can't convert pathnames with wild components.
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name)) ; Verifying if pathname isn't already in directory form before converting.
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname))) ; Appending the filename to the directory component of the pathname after the previous directories, if given, or the keyword :relative if not.
	 :name     nil
	 :type     nil
	 :defaults pathname)
	pathname)))

(defun directory-wildcard (dirname)
  "Creates a wildcard for a given directory"
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  "Lists the subdirectories in a directory in CLISP"
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun list-directory (dirname)
  "Lists files and subdirectories in a directory"
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl ; In OpenMCL we need to specify :directories to get the subdirectories.
    (directory wildcard :directories t) 

    #+allegro ; In Allegro we need to specify :directories-are-files to get subdirectories in directory form
    (directory wildcard :directories-are-files nil) 

    #+clisp ; In CLisp we need to make one call for subdirectories and another for files
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

(defun pathname-as-file (name)
  "Converts a pathname to file form."
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (directory-pathname-p name) ; Verifying if the given path is in directory form before converting.
	(let* ((directory (pathname-directory pathname)) ; Obtaining the directory from the directory form
	       (name-and-type (pathname (first (last directory))))) ; Obtaining name and type from the last component in directory.
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	pathname)))

(defun file-exists-p (pathname)
  "Verifies if a given path exists"
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu) ; In Allegro and CMU files must be passed in file form and directories in directory form, otherwise they won't be found.
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp ; CLisp signals errors if it finds files in directory form and vice-versa.
  (or (ignore-errors
	(probe-file (pathname-as-file pathname)))
      (ignore-errors
	(let ((directory-form (pathname-as-directory pathname)))
	  (when (ext:probe-directory directory-form)
	    directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "list-directory not implemented"))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name) ; Recursive function that will be applied to dirname
	 (cond
	   ((directory-pathname-p name) ; Condition 1
	    (when (and directories (funcall test name)) ; Verifying if directories is set to T and applying the test to the current directory.
	      (funcall fn name))
	    (dolist (x (list-directory name)) (walk x))) ; Walking each file inside the given one
	   ((funcall test name) ; Condition 2
	    (funcall fn name)))))
    (walk (pathname-as-directory dirname))))
