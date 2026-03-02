(load (merge-pathnames "example_tools.lisp"
                       (or *load-pathname* *default-pathname-defaults*)))

(defpackage #:claude-test
  (:use #:cl #:llm #:claude))

(in-package #:claude-test)

(format t "~%--- Testing Claude Completions ---~%")
(let ((response (claude:completions "Why is the sky blue?")))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Claude Tool Calling ---~%")
(let ((response (claude:completions "What is the weather in Paris, France in celsius?" '("get-weather"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Claude Tool Calling: add-numbers ---~%")
(let ((response (claude:completions "What is 42 plus 58?" '("add-numbers"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Claude Tool Calling: get-current-time ---~%")
(let ((response (claude:completions "What is the current time?" '("get-current-time"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Claude Tool Calling: capitalize-text ---~%")
(let ((response (claude:completions "Please capitalize the text 'hello world'" '("capitalize-text"))))
  (format t "Response:~%~A~%" response))
