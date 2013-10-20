(in-package :cl-user)

;;; Suggested start up file for EECS 325
;;; Platform: LispWorks on Windows and MacOS

;;; Update History
;;;
;;; 09/23/2011 Clarified comments on directory changing [CKR]

#-quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

;;; Set the depth of trace output to a large but finite level.
;;(setq hcl:*trace-print-level* 10)

;;; Turn off backup files
;;;(setf (editor:variable-value 'editor::backups-wanted) nil)

;;; Switches the default directory to the one containing this file
;; (change-directory
;;  (make-pathname :directory (pathname-directory *load-truename*)))


(ql:quickload "aserve")
(ql:quickload "webactions")

;;; Load cs325.lisp to create the cs325 package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "cs325.lisp"))

(in-package #:cs325-user)

(defun fib-tail (n &optional (curr 1) (next 1))
  (if (< n 2)
      curr
      (fib-tail (1- n) next (+ curr next))))

(defun greater (a b)
  (if (> a b) a b))

(defun has-list-p (list)
  (cond
    ((null list) nil)
    (t (or (listp (first list))
	   (has-list-p (rest list))))))

(defun print-dots (n)
  (loop for i from 1 to n
        do (princ ".")))

(defun get-a-count (list)
  (count-if #'(lambda (x) (eql 'a x)) list))

(defun summit (list)
  (let ((result (remove nil list)))
    (apply #'+ result)))



