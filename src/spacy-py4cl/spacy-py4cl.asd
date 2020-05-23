;;;; spacy-py4cl.asd

(asdf:defsystem #:spacy-py4cl
  :description "Use py4cl to use Python spaCy library embedded in Common Lisp"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:py4cl)
  :serial t
  :components ((:file "package")
               (:file "spacy-py4cl")))
