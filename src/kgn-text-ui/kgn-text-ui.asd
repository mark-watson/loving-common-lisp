;;;; kgn-text-ui.asd

(asdf:defsystem #:kgn-text-ui
  :description "top level text UI for the Knowledge Graph Navigator package"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:kgn-common #:sparql #:kbnlp #:myutils)
  :components ((:file "package")
                (:file "kgn-text-ui")))

