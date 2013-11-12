;; simple app that uses rot_13 to encrypt the entered text.

(ql:quickload '(:hunchentoot :cl-who ))
 
(defpackage #:cs253-2
  (:use :cl :hunchentoot :cl-who))

(in-package #:cs253-2)

(defmacro rot-form (storevalue)
  `(with-html-output (*standard-output* nil)
     (:html
      (:head
       (:title "rot13"))
      (:body
       (:h3 "Encrypt extremely cleverly text")
       (:form :method "post"
	      (:input :name "store" :value ,storevalue)
	      (:input :type "submit"))))))


(define-easy-handler (main :uri "/rot13" :default-request-type :post)
    (store)
  (with-html-output-to-string (*standard-output* nil)
    (rot-form (rot13 store))))

(defvar *web-server* (make-instance 'easy-acceptor :port 4343))


(defun rot13 (string)
  (map 'string
       (lambda (char &aux (code (char-code char)))
         (if (alpha-char-p char)
             (if (> (- code (char-code (if (upper-case-p char)
                                           #\A #\a))) 12)
                 (code-char (- code 13))
                 (code-char (+ code 13)))
             char))
       string))
 



