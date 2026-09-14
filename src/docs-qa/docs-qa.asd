;;;; docs-qa.asd

(asdf:defsystem #:docs-qa
  :description "Library for Documents QA using OpenAI APIs via the litelm routing library"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:litelm #:sqlite #:split-sequence)
  :components ((:file "package")
               (:file "docs-qa")))

