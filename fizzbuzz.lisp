(load "~/quicklisp/setup.lisp")
(ql:quickload "lisp-unit")

(lisp-unit:define-test for-2
  (lisp-unit:assert-equal 2 (fizzbuzz 2)))

(lisp-unit:define-test for-3
  (lisp-unit:assert-equal "Fizz" (fizzbuzz 3)))

(lisp-unit:define-test for-5
  (lisp-unit:assert-equal "Buzz" (fizzbuzz 5)))

(lisp-unit:define-test for-7
  (lisp-unit:assert-equal 7 (fizzbuzz 7)))

(lisp-unit:define-test for-1
  (lisp-unit:assert-equal 1 (fizzbuzz 1)))

(lisp-unit:define-test for-30
  (lisp-unit:assert-equal "FizzBuzz" (fizzbuzz 30)))




(defun fizzbuzz (n)
  (cond
    ((and (zerop (mod n 3))
	  (zerop (mod n 5)))
     "FizzBuzz")
    ((zerop (mod n 3))
     "Fizz")
    ((zerop (mod n 5))
     "Buzz")
    (t n)))

(defun one-to-hundred ()
  (loop
       for i from 1 to 100
       do (print (fizzbuzz i))))
