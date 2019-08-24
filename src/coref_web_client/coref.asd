;;;; webscrape.asd

(asdf:defsystem #:coref
  :description "Describe coref here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma #:do-urlencode #:flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "coref")))
