(in-package #:groq)

(defvar *groq-model* "openai/gpt-oss-120b") ;; or "llama3-70b-8192"

(defun groq-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (cl-json:decode-json s))))

(defun groq-completion (content)
  (let* ((url "https://api.groq.com/openai/v1/chat/completions")
         (api-key (uiop:getenv "GROQ_API_KEY"))
         (data
           (cl-json:encode-json-to-string
            `((:model . ,*groq-model*)
              (:messages . (((:role . "system")
                             (:content . "You are a helpful assistant."))
                            ((:role . "user")
                             (:content . ,content)))))))
         (curl-command
           (concatenate
            'string
            "curl -s " url
            " -H \"Content-Type: application/json\""
            " -H \"Authorization: Bearer " api-key "\" "
            " -d '" data "'")))
    (groq-helper curl-command)))

#|
(groq:groq-completion "Write a Common Lisp framework for LLM MCP: write three things: 1. server side MCP hub. 2. client library. 3. an example program. Note that MCP is Model Context Protocol.")
(groq:groq-completion "Sally is 77, Bill is 32, and Alex is 44 years old. Pairwise, what are their age differences? Print results in JSON format. Be concise and only provide a correct answer, no need to think about different correct answers.")

(setf xx (groq:groq-completion "Sally is 77, Bill is 32, and Alex is 44 years old. Pairwise, what are their age differences? Print results in JSON format. Be concise and only provide a correct answer, no need to think about different correct answers."))
(groq:groq-extract-content xx)
|#

(defun groq-extract-content (resp)
  (cdr (nth 2 (cadr (cadr (assoc :choices resp))))))

(defun testg ()
  (let ((resp (groq-completion "Write a Common Lisp framework for LLM MCP: write three things: 1. server side MCP hub. 2. client library. 3. an example program. Note that MCP is Model Context Protocol.")))
    (print resp)
    (terpri)
    (print (groq-extract-content resp))))

(defun user-input ()
  (loop
     (format t "Enter your input (type 'quit' or 'exit' to stop): ")
     (let ((input (read-line)))
       (if (member input '("quit" "Quit" "exit" "Exit") :test 'string=)
           (return)
           (let ((response (groq-completion input)))
             (format t "~A~%" (groq-extract-content response)))))))
