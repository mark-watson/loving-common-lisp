;;;; ollama.asd

(asdf:defsystem #:ollama
  :description "Library for using the ollama APIs via litelm"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:litelm #:uiop)
  :components ((:file "package")
               (:file "ollama-helper")
               (:file "ollama-tools") 
               (:file "ollama")
               (:file "ollama-cloud-search")))

