(in-package :chapter-24-package)

;; Constants:

(defconstant +null+ (code-char 0))

;; Variables:

(defvar *in-progress-objects* nil)

;; Generic functions:

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

;; Macros:

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  "Wraps up the generic code for defining binary classes. Only the read-method must be defined specially for each class."
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get ',name 'slots) ',(mapcar #'first slots))
	 (setf (get ',name 'superclasses) ',superclasses))

       (defclass ,name ,superclasses
	 ,(mapcar #'slot->defclass-slot slots))

       ,read-method

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
		  (declare (ignorable ,streamvar))
		  (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
		    ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))


(defmacro define-binary-class (name (&rest superclasses) slots)
  "A convenient way to define a class and its methods for a binary type."
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
					 (defmethod read-object progn ((,objectvar ,name) ,streamvar)
						    (declare (ignorable ,streamvar))
						    (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
						      ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  "Abstracts tagged binary data so it can be manipulated in the code."
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
				  (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
				    (let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
				      (let ((,objectvar
					     (make-instance
					      ,@(or (cdr (assoc :dispatch options))
						    (error "Must supply :dispatch form."))
					      ,@(mapcan #'slot->keyword-arg slots))))
					(read-object ,objectvar ,streamvar)
					,objectvar))))))

(defmacro define-binary-type (name (&rest args) &body spec)
  "Generates read-value and write-value methods for existing classes rather than defined with define-binary-class."
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
	 `(eval-when (:compile-toplevel :load-toplevel :execute)
	    (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
	      (read-value ',derived-from ,stream ,@derived-args))
	    (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
	      (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
	  ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
	     `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
		,@body))
	  ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
	     `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
		,@body)))))))
  
;; Functions:

(defun slot->defclass-slot (spec)
  "Converts a slot from binary-class to a slot in defclass."
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  "Converts a slot from binary-class to a field in the read-value method."
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  "Converts a slot from binary-class to an expression for a write-value call."
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun direct-slots (name)
  "Returns the slots bound to a binary class."
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  "Returns the slots bound to the superclasses of a binary class."
  (loop for super in (get name 'superclasses)
     nconc (direct-slots super)
     nconc (inherited-slots super)))

(defun all-slots (name)
  "Concatenates a binary class' direct and inherited slots."
  (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  "Does the same as all-slots but can be used within the define-binary-class macro."
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defun slot->binding (spec stream)
  "Converts an abstracted slot to a binding to a read-value call, which is used in a let, most likely."
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

;; Methods

(defmethod read-object :around (object strream)
  "Around method that pushes the object being manipulated into *in-progress-objects*."
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  "Around method that pushes the object being manipulated into *in-progress-objects*."
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

;; Auxiliary:

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  "Auxiliary function to parse the specs the same way when there are or aren't keywords in them. '(x y) and '(x (y)) should both return '(x (y))."
  (list (first spec) (mklist (second spec))))

(defun as-keyword (sym)
  "Translates a symbol to the corresponding keyword."
  (intern (string sym) :keyword))
