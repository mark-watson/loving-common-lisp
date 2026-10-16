;;;; fasttag.asd

(asdf:defsystem #:fasttag
  :description "FastTag rule-based part of speech tagger"
  :author "Mark Watson"
  :license "AGPL-3.0 or Apache 2"
  :depends-on (#:myutils)
  :serial t
  :components ((:file "package")
               (:file "fasttag")))

