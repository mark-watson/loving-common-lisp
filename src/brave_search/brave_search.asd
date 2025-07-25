;;;; brave_search.asd

(asdf:defsystem #:brave_search
  :description "brave_search api"
  :author "markw@markwatson.com"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:drakma #:myutils)
  :components ((:file "package")
               (:file "brave_search")))
