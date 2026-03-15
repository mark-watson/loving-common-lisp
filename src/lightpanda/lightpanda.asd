(asdf:defsystem #:lightpanda
  :description "Common Lisp interface to the Lightpanda headless browser"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on (#:cl-json)
  :components ((:file "lightpanda")
               (:file "project" :depends-on ("lightpanda"))))
