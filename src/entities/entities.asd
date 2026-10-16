;;;; entities.asd

(asdf:defsystem #:entities
  :description "Dictionary-based named entity recognition"
  :author "Mark Watson"
  :license "AGPL-3.0 or Apache 2"
  :depends-on (#:myutils)
  :serial t
  :components ((:file "package")
               (:file "entities")))

