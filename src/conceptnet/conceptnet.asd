;;;; conceptnet.asd

(asdf:defsystem #:conceptnet
  :description "conceptnet"
  :author "Mark Watson <markw@markwatson.com>"
  :license "GPL - since it depends on GPL libraries"
  :depends-on (#:uiop #:drakma #:cl-json)
  :components ((:file "package")
               (:file "conceptnet")))
