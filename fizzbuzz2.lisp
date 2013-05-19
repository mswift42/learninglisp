;; Extend Fizzbuzz to print fizz for every number with a 3 in it.
(load "~/quicklisp/setup.lisp")
(ql:quickload "lisp-unit")

(defun fizzbuzz (n)
  "check if divisible by 5 and/or 3."
  (cond
    ((and (zerop (mod n 3))
	  (zerop (mod n 5)))
     "FizzBuzz")
    ((zerop (mod n 3))
     "Fizz")
    ((zerop (mod n 5))
     "Buzz")
    (t n)))

(defun number-to-list (nr)
  "Convert Integer to a list with the single digits. 
   (number-to-list 123) -> '(1 2 3)"
  (map 'list #'digit-char-p (prin1-to-string nr)))

(defun fizzbuzz-extended (nr)
  "When 3 in nr and divisible by 5 return fizzbuzz,
   else check for 3 in nr. if not return normal fizzbuzz."
  (cond
    ((and (member 3 (number-to-list nr))
	  (= 0 (mod nr 5)))
     "FizzBuzz")
    ((member 3 (number-to-list nr))
     "Fizz")
    (t (fizzbuzz nr))))

(defun solution ()
  "Print fizzbuzz for range 1,101"
  (loop
       for i from 1 to 100
       do (print (fizzbuzz-extended i))))

(lisp-unit:define-test fizz
  (lisp-unit:assert-equal 1 (fizzbuzz 1))
  (lisp-unit:assert-equal 2 (fizzbuzz 2))
  (lisp-unit:assert-equal "Fizz" (fizzbuzz 3))
  (lisp-unit:assert-equal "Buzz" (fizzbuzz 5))
  (lisp-unit:assert-equal 7 (fizzbuzz 7))
  (lisp-unit:assert-equal "FizzBuzz" (fizzbuzz 15)))

(lisp-unit:define-test test-number-to-list
  (lisp-unit:assert-equal '(1 3 3) (number-to-list 133))
  (lisp-unit:assert-equal '(5) (number-to-list 5))
  (lisp-unit:assert-equal '(1 2 3 1 2 3) (number-to-list 123123)))

(lisp-unit:define-test test-fizzbuzz-extended
  "test extended version of fizzbuzz. For 35 (has 3 in it 
   and is divisible by 5) return FizzBuzz"
  (lisp-unit:assert-equal 1 (fizzbuzz-extended 1))
  (lisp-unit:assert-equal "Fizz" (fizzbuzz-extended 3))
  (lisp-unit:assert-equal "Buzz" (fizzbuzz-extended 5))
  (lisp-unit:assert-equal "Fizz" (fizzbuzz-extended 13))
  (lisp-unit:assert-equal "FizzBuzz" (fizzbuzz-extended 15))
  (lisp-unit:assert-equal "FizzBuzz" (fizzbuzz-extended 35)))4


