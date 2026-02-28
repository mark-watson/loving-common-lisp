;;;; knowledgegraphnavigator.asd

(asdf:defsystem #:kgn-capi-ui
  :description "top level Knowledge Graph Navigator package"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:kgn-common #:sparql #:kbnlp #:lw-grapher #:trivial-open-browser)
  :components ((:file "package")
                (:file "kgn-capi-ui")
                (:file "option-pane")
                ;;(:file "ui-text")
                (:file "colorize")
                (:file "user-interface")))

