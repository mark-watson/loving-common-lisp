(in-package #:openai)

(defun groq-completion (content)
  (let* ((url "https://api.groq.com/openai/v1/chat/completions")
         (api-key (uiop:getenv "GROQ_API_KEY"))
         (headers `(("Authorization" . ,(concatenate 'string "Bearer " api-key))
                    ("Content-Type" . "application/json")))
         (data `(("model" . "meta-llama/llama-4-scout-17b-16e-instruct") ;; "llama3-70b-8192")
		 ("messages" . ((("role" . "system")
				 ("content" . "content"))
                                (("role" . "user")
				 ("content" . ,content))))))
         (json-data (cl-json:encode-json-to-string data)))
    (cl-json:decode-json-from-string
     (flexi-streams:octets-to-string
      (drakma:http-request url
                           :method :post
                           :content-type "application/json"
                           :additional-headers headers
                           :content json-data)))))

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
