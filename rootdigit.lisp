;; sum digits of a number to a single digit.
(load "~/quicklisp/setup.lisp")
(ql:quickload "lisp-unit")


(lisp-unit:define-test test-number-to-list
  (lisp-unit:assert-equal '(2 3 3) (number-to-list 233))
  (lisp-unit:assert-equal '(4 5 1) (number-to-list 451)))

(lisp-unit:define-test test-sum-list
  (lisp-unit:assert-equal 10 (sum-list '(2 2 3 3)))
  (lisp-unit:assert-equal 8 (sum-list '(1 2 3 2))))

(lisp-unit:define-test test-root-digits
  (lisp-unit:assert-equal 8 (root-digits '(3 1 3 3 7))))

(lisp-unit:define-test test-result
  (lisp-unit:assert-equal 1 (result 1073741824)))


(defun number-to-list (nr)
  "Convert number to list of its digits."
  (map 'list #'digit-char-p (prin1-to-string nr)))

(defun sum-list (lst)
  "Sum numbers in a list"
  (reduce #'+ lst))

(defun root-digits (lst)
  "Recursively get the root digit of a number."
  (cond
    ((null lst) 0)
    ((< (sum-list lst) 10) (sum-list lst))
    (t (root-digits (number-to-list (sum-list lst)))))) 

(defun result (nr)
  (root-digits (number-to-list nr)))







