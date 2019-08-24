;;;; kgcreator.asd

(asdf:defsystem #:kgcreator
  :description "Describe plotlib here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:entities_dbpedia #:categorize_summarize #:myutils #:unix-opts #:cl-who #:hunchentoot #:parenscript)
  :components
    ((:file "package")
                (:file "kgcreator")
                (:file "neo4j")
                (:file "rdf")
                (:file "web"))
                 )

