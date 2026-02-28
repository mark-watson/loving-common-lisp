;;;; docs-qa.asd

(asdf:defsystem #:docs-qa
  :description "Library for Documents QA using OpenAI APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:openai #:sqlite #:split-sequence)
  :components ((:file "package")
               (:file "docs-qa")))

