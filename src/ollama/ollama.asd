;;;; ollama.asd

(asdf:defsystem #:ollama
  :description "Library for using the ollama APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "ollama")))

