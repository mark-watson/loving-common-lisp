;;;; bing.asd

(asdf:defsystem #:bing
  :description "Describe dbpedia here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:uiop #:cl-json #:drakma #:myutils)
  :components ((:file "package")
               (:file "bing")))
