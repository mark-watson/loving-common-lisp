(defsystem "cache-engine"
  :version "0.1.0"
  :author "Mark Watson"
  :license "Apache 2.0"
  :depends-on ("sqlite")
  :components ((:file "cache-engine"))
  :description "Persistent LLM cache using SQLite.")
