;;;; docs-qa.asd

(asdf:defsystem #:docs-qa
  :description "Library for Documents QA using OpenAI APIs via the litelm routing library"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:litelm #:sqlite)
  :components ((:file "package")
               (:file "docs-qa")))

