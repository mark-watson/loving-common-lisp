;;;; knowledgegraphnavigator.asd

(asdf:defsystem #:kgn
  :description "Describe dbpedia here"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:sqlite #:cl-json #:alexandria #:drakma #:myutils #:lw-grapher #:trivial-open-browser #:entities #:entity-uris #:kbnlp)
  :components ((:file "package")
               (:file "ui-text")
               (:file "utils")
               (:file "sparql")
               (:file "colorize")
               (:file "user-interface")
               (:file "option-pane")
               (:file "kgn")
               (:file "gui")
               (:file "nlp")
               (:file "sparql-results-to-english")
               (:file "gen-output")))

