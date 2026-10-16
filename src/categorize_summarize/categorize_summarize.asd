;;;; categorize_summarize.asd

(asdf:defsystem #:categorize_summarize
  :description "Text categorization and summarization"
  :author "Mark Watson"
  :license "AGPL-3.0 or Apache 2"
  :depends-on (#:myutils #:fasttag)
  :serial t
  :components ((:file "package")
               (:file "categorize_summarize")))

