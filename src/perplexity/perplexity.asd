;;;; openai.asd

(asdf:defsystem #:perplexity
  :description "Library for using the perplexity search+LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:drakma)
  :components ((:file "package")
               (:file "perplexity")))
