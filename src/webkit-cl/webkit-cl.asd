;;; webkit-cl.asd — ASDF system definition for webkit-cl
;;;
;;; A lightweight Common Lisp framework for building native macOS
;;; desktop applications with WebKit (WKWebView).

(asdf:defsystem #:webkit-cl
  :description "Lightweight WebKit GUI apps for Common Lisp (macOS)"
  :author "Mark Watson"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on (#:cffi #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "webkit-cl-ffi")
               (:file "bridge")
               (:file "webkit-cl")))
