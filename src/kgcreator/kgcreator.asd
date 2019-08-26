;;;; kgcreator.asd

(asdf:defsystem #:kgcreator
  :description "Describe plotlib here"
  :author "Mark Watson <mark.watson@gmail.com>"
  :license "AGPL version 3"
  :depends-on (#:entities_dbpedia #:categorize_summarize #:myutils #:unix-opts #:cl-who #:hunchentoot #:parenscript)
  :components
    ((:file "package")
                (:file "kgcreator")
                (:file "neo4j")
                (:file "rdf")
                (:file "web"))
                 )

