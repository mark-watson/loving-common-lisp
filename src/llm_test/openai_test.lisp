(load (merge-pathnames "example_tools.lisp"
                       (or *load-pathname* *default-pathname-defaults*)))

(defpackage #:openai-test
  (:use #:cl #:llm #:openai))

(in-package #:openai-test)

(format t "~%--- Testing OpenAI Completions ---~%")
(let ((response (openai:completions "Why is the sky blue?")))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing OpenAI Tool Calling ---~%")
(let ((response (openai:completions "What is the weather in Paris, France in celsius?" :tools '("get-weather"))))
  (format t "Response:~%~A~%" response))
