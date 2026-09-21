;;;; daily-use.asd

(asdf:defsystem #:daily-use
  :description "Interactive REPL for Gemini with search grounding and a persistent cache"
  :author "Mark Watson"
  :license "Apache-2.0"
  :depends-on (#:litelm        ; provider-neutral chat completions
               #:gemini        ; Google Search grounding (a Gemini-native tool)
               #:cache-engine  ; SQLite-backed persistent cache
               #:cl-readline)  ; line editing and history
  :components ((:file "daily-use")))
