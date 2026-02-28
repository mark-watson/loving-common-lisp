;;; test.lisp -- usage examples for the ollama Common Lisp library

;;; Load and initialize the library via Quicklisp
(format t "Loading ollama library...~%")
(ql:quickload :ollama)

(format t "~%=== Completion Example ===~%")
(format t "~a~%" (ollama:completions "Hello, how are you?"))

(format t "~%=== Summarize Example ===~%")
(format t "~a~%"
         (ollama:summarize
          "Common Lisp is a powerful, dynamic language with macros and extensibility."))

(format t "~%=== Answer Question Example ===~%")
(format t "~a~%" (ollama:answer-question "What is the capital of France?"))

(format t "~%=== Answer Question Example ===~%")
(format t "~a~%" (ollama::completions "Use function calling for: What's the weather like in New York?" '("get_weather" "calculate")))
