;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; MIT License

(asdf:defsystem #:search-apis
  :description "One common web search interface across providers (Brave, Tavily, Perplexity). Modelled after the litelm library: a small provider registry, a shared result format, and a self-contained JSON codec so the only dependencies are dexador and quri."
  :author "Mark Watson"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:dexador #:quri #:uiop)
  :components ((:file "package")
               (:file "json")
               (:file "search-apis")
               (:file "providers")))
