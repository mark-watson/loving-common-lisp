(ql:quickload :hunchentoot)

(defvar *h* (make-instance 'hunchentoot:easy-acceptor :port 3000))

;; define a handler with the arbitrary name my-greetings:

(hunchentoot:define-easy-handler (my-greetings :uri "/hello") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hello ~A" name))

(hunchentoot:start *h*)
