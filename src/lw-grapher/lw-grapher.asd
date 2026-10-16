;;;; lw-grapher.asd

(asdf:defsystem :lw-grapher
  :description "Simple graph layout and display for LispWorks CAPI"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:myutils)
  :components ((:file "package")
               (:file "text-node")
               (:file "grapher")
               (:file "info-pane-grapher")))

