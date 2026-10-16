;;;; myutils.asd

(asdf:defsystem #:myutils
  :description "String and file utilities shared by the book examples"
  :author "Mark Watson"
  :license "AGPL-3.0 or Apache 2"
  ;;:depends-on (#:drakma #:babel #:s-xml)
  :components ((:file "package")
               (:file "myutils")))

