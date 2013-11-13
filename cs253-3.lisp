;;; Implement a Blog in Common Lisp
(ql:quickload '(:hunchentoot :cl-who :sqlite))

(defpackage #:cs253-3
  (:use :cl :hunchentoot :cl-who))

(in-package #:cs253-3)

;; tell hunchentoot which css file to use.
(push (create-static-file-dispatcher-and-handler
       "/blog.css" "blogstyle.css") *dispatch-table*)

(defmacro with-html (&body body)
  `(with-html-output (*standard-output* nil :prologue t :indent t)
     ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))

(defmacro page-template (title &body body)
  `(with-html-string
     (:html
      (:head
       (:title ,title)
       (:link :rel "stylesheet" :type "text/css" :href "/blog.css"))
      (:body ,@body))))

(define-easy-handler (index :uri "/index")
    ()
    (page-template "Index"
	(:h3 :class "header" "this is a header")))

(define-easy-handler (new-post :uri "/newpost")
    (post title)
  (with-html-string
    (:p (str post))
    (:p (str title))))

(defvar *web-server*
  (make-instance 'easy-acceptor :port 4444))













