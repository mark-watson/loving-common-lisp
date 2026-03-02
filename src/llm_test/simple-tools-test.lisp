(load (merge-pathnames "example_tools.lisp"
                       (or *load-pathname* *default-pathname-defaults*)))

(defpackage #:simple-tools-example
  (:use #:cl #:simple-tools))

(in-package #:simple-tools-example)

;; Example of calling tools
(format t "~%--- Testing Simple Tools ---~%")

;; Call the tools directly
(format t "~A~%" (call-tool "add-numbers" 5 3))
(format t "~A~%" (call-tool "get-current-time"))
(format t "~A~%" (call-tool "capitalize-text" "hello world"))

;; Show all defined tools
(format t "~%Defined tools:~%")
(loop for key being the hash-keys of simple-tools:*tools* do
  (format t "  - ~A~%" key))
