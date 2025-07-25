;;;; openai.asd

(asdf:defsystem #:tavily
  :description "Library for using the perplexity search+LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:dexador :jonathan)
  :components ((:file "package")
               (:file "tavily")))
