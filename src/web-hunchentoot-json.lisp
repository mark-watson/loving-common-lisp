(ql:quickload :hunchentoot)
(ql:quickload :cl-json)

(defvar *h* (make-instance 'hunchentoot:easy-acceptor :port 3000))

;; define a handler with the arbitrary name my-greetings:

(hunchentoot:define-easy-handler (animal :uri "/animal") (name)
  (print name)
  (setf (hunchentoot:content-type*) "text/plain")
  (cond
    ((string-equal name "cat")
     (json:encode-json-to-string
       (list
        (list
         '(average_weight . 10)
         '(friendly . nil))
       "A cat can live indoors or outdoors.")))
    ((string-equal name "dog")
     (json:encode-json-to-string
       (list
        (list
         '(average_weight . 40)
         '(friendly . t))
       "A dog is a loyal creature, much valued by humans.")))
    (t
     (json:encode-json-to-string
       (list
        ()
       "unknown type of animal")))))

(hunchentoot:start *h*)
