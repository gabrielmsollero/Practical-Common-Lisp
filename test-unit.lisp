(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun report-result (result form)
  "Evaluates an expression which returns a boolean and prints if it failed or passed. Returns a boolean."
  (format t "~:[FAIL~;pass~] ...~a: ~a~%" result *test-name* form)
  result)


(defmacro combine-results (&body forms)
  "Evaluates the tests passed and returns a boolean which is T only if all the tests are T. It's similar to AND, but it doesn't stop when it finds a NIL."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  "Applies combine-results to the tests through report-result so that they can be printed. Returns the result given by combine-results."
  `(combine-results
     ;; Running all the report-results the macro will print the tests and return a boolean in the end that indicates if any test failed.
     ;; If the final value is T, there is no need to verify all the tests.
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  "Defines a test function in which we can call other tests or run particular tests."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
      (= (* 2 2) 4)
      (= (* 3 5) 15)))

(deftest test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))
