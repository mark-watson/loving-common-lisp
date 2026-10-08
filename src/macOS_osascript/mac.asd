;;;; mac.asd -- ASDF system for the :mac package.

(asdf:defsystem #:mac
  :description "macOS automation via osascript (Mail, Calendar, Messages, Reminders, Safari)."
  :depends-on (#:uiop)
  :serial t
  :components ((:file "package")
               (:file "osascript")
               (:file "read-recent-imessages")
               (:file "safari-login")))
