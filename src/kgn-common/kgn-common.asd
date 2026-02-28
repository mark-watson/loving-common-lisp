;;;; knowledgegraphnavigator.asd

(asdf:defsystem #:kgn-common
  :description "common utilities for Knowledge Graph Navigator"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:sqlite #:cl-json #:alexandria #:drakma #:myutils #:entities #:entity-uris #:kbnlp #:entity-uris #:sparql-cache)
  :components ((:file "package")
               (:file "utils")
               (:file "kgn-common")))

