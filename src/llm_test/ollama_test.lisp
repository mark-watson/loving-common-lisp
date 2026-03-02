(load (merge-pathnames "example_tools.lisp"
                       (or *load-pathname* *default-pathname-defaults*)))

(defpackage #:ollama-test
  (:use #:cl #:llm #:ollama))

(in-package #:ollama-test)

(format t "~%--- Testing Ollama Completions ---~%")
(let ((response (ollama:completions "Be very concise: Why is the sky blue?"
				    :think nil)))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Ollama Tool Calling ---~%")
(let ((response (ollama:completions "What is the weather in Paris, France in celsius?"
				    :tools '("get-weather"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Ollama Tool Calling: add-numbers ---~%")
(let ((response (ollama:completions "What is 42 plus 58?"
				    :tools'("add-numbers"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Ollama Tool Calling: get-current-time ---~%")
(let ((response (ollama:completions "What is the current time?"
				    :tools '("get-current-time"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Ollama Tool Calling: capitalize-text ---~%")
(let ((response (ollama:completions "Please capitalize the text 'hello world'"
				    :tools'("capitalize-text"))))
  (format t "Response:~%~A~%" response))
