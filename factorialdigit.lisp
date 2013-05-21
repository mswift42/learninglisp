;; get Last non zero digit of a factorial number
(load "~/quicklisp/setup.lisp")
(ql:quickload "lisp-unit")

(lisp-unit:define-test test-factorial
    (lisp-unit:assert-equal 120 (factorial 5)))

(defun factorial (n)
  (reduce #'* (loop for i from 1 to n collect i)))

(lisp-unit:define-test test-number-to-list
  (lisp-unit:assert-equal '(2 3 4) (number-to-list 234))
  (lisp-unit:assert-equal '(3 4 1) (number-to-list 341)))

(defun number-to-list (n)
  "Conert number to list of number's digits."
  (map 'list #'digit-char-p (prin1-to-string n)))

(lisp-unit:define-test test-non-zero
  (lisp-unit:assert-equal 2 (non-zero (number-to-list (factorial 5)))))

(defun non-zero (lst)
  "return the last non-zero digit in a list."
  (cond
    ((null lst) nil)
    ((zerop (first lst)) nil)
    ((zerop (second lst)) (first lst))
    (t (non-zero (rest lst)))))

(defun result (n)
  (non-zero (number-to-list (factorial n))))


