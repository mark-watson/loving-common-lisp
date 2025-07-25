;;;; kbnlp.asd

(asdf:defsystem #:kbnlp
  :description "Mark Watson's old Common Lisp NLP library"
  :author "Mark Watson <markw@markwatson.com>"
  :license "use either: Apache 2, or: LGPL v3"
  :depends-on (#:myutils #:fasttag #:entity-uris)
  :serial t
  :components ((:file "package")
               (:file "kbnlp")))

