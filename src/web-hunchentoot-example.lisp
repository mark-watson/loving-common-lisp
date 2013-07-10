(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(in-package :cl-user)
(defpackage hdemo
  (:use :cl
        :cl-who
        :hunchentoot))
(in-package :hdemo)

(defvar *h* (make-instance 'easy-acceptor :port 3000))

;; define a handler with the arbitrary name my-greetings:

(define-easy-handler (my-greetings :uri "/hello") (name)
  (setf (hunchentoot:content-type*) "text/html")
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "hunchentoot test"))
     (:body
      (:h1 "hunchentoot form demo")
      (:form
       :method :post
       (:input :type :text
	       :name "name"
	       :value name)
       (:input :type :submit :value "Submit your name"))
      (:p "Hello " (str name))))))

(hunchentoot:start *h*)
