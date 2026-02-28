;;;; kgsampler.asd

(asdf:defsystem #:kgsampler
  :description "sample knowledge graphs"
  :author "Mark Watson markw@markwatson.com"
  :license "Apache 2"
  :depends-on (#:uiop #:drakma #:sparql)
  :components ((:file "package")
               (:file "kgsampler")))

