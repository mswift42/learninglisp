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


(defun number-to-list (nr)
  (map 'list #'digit-char-p (prin1-to-string nr)))

(defun sum-list (lst)
  (reduce #'+ lst))

(defun root-digits (lst)
  (cond
    ((null lst) 0)
    ((< (sum-list lst) 10) (sum-list lst))
    (t (sum-list (number-to-list (sum-list lst))))))








