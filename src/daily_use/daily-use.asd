;;;; daily-use.asd

(asdf:defsystem #:daily-use
  :description "Interactive REPL for Gemini AI with search grounding and persistent cache"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:gemini #:cache-engine #:cl-readline #:cl-json)
  :components ((:file "daily-use")))
