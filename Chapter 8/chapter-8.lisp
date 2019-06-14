(in-package :chapter-8-package)

(defun primep (number)
  "Verifies if number is prime by looking for divisors from 2 to its square root."
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Finds the smallest prime larger than number."
  (loop for n from number when (primep n) return n))

;; Creating a macro with the following code expected as expansion:
;; (do-primes (p 0 19)
  ;; (format t "~d " p)) -> 2 3 5 7 11 13 17 19

(defmacro do-primes ((var start end) &body body)
  "Iterates var from start to end (both included) assuming only prime values"
  (let ((ending-value-name (gensym))) ; preventing leaks
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
       ((> ,var ,ending-value-name))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  "Generates new simbols for all the names in list to prevent name conflict."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
