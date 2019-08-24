;;;; webscrape.asd

(asdf:defsystem #:webscrape
  :description "Describe webscrape here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma)
  :serial t
  :components ((:file "package")
               (:file "webscrape")))

