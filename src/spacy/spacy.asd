;;;; spacy.asd

(asdf:defsystem #:spacy
  :description "Describe spacy here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:py4cl)
  :serial t
  :components ((:file "package")
               (:file "spacy")))