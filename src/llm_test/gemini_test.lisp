(load (merge-pathnames "example_tools.lisp"
                       (or *load-pathname* *default-pathname-defaults*)))

(defpackage #:gemini-test
  (:use #:cl #:llm #:gemini))

(in-package #:gemini-test)

(format t "~%--- Testing Gemini Completions ---~%")
(let ((response (gemini:generate "Why is the sky blue?")))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Gemini Completions With Google Search ---~%")
(let ((response (gemini:generate-with-search "What sci-fi movies are playing in Flagstaff Arizona today?")))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Gemini Tool Calling: get-weather ---~%")
(let ((response (gemini:generate "What is the weather in Paris, France in celsius?" '("get-weather"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Gemini Tool Calling: add-numbers ---~%")
(let ((response (gemini:generate "What is 42 plus 58?" '("add-numbers"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Gemini Tool Calling: get-current-time ---~%")
(let ((response (gemini:generate "What is the current time?" '("get-current-time"))))
  (format t "Response:~%~A~%" response))

(format t "~%--- Testing Gemini Tool Calling: capitalize-text ---~%")
(let ((response (gemini:generate "Please capitalize the text 'hello world'" '("capitalize-text"))))
  (format t "Response:~%~A~%" response))
