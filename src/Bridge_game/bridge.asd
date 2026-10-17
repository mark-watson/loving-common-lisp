;;;; bridge.asd — ASDF System Definition for Bridge Game

(asdf:defsystem #:bridge
  :description "Contract Bridge AI — text-based bridge game with AI opponents"
  :author "Mark Watson"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "cards")
               (:file "bidding")
               (:file "display")
               (:file "play")
               (:file "scoring")
               (:file "engine")))
