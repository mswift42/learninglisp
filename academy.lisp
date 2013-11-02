;; Solutions to https://github.com/whalliburton/academy
;;
(ql:quickload :fiveam)
(defpackage #:academy
  (:use :cl :fiveam)) 

(in-package #:academy)

(defun random-element (list)
  (let ((len (length list)))
    (nth (random len) list)))

(test test-ensure-list
  (is (equal '(a) (ensure-list 'a)))
  (is (equal '(a) (ensure-list '(a)))))

(defun ensure-list (exp)
  "ensure that expression is a list"
  (if
   (listp exp) exp
   (list exp)))

(run!)
