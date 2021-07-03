(in-package #:openai)

;;(print (uiop:getenv "OPENAI_KEY")) ;; define this environment variable with the value of your OpenAI API key

(defun completions (starter-text max-tokens
                    &aux (host "https://api.openai.com/v1/engines/davinci/completions"))
  (let* ((curl-command
          (concatenate
           'string
           "curl " host
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" " 
           host
           " -d '{\"prompt\": \"" starter-text "\", \"max_tokens\": "
           (write-to-string max-tokens)  "}'"))
         (response
          (uiop:run-program
           curl-command
            :output :string)))
      (with-input-from-string
          (s response)
        (let* ((json-as-list (json:decode-json s)))
          ;; extract text (this might change if OpenAI changes JSON return format):
          (cdar (cadr (nth 4 json-as-list)))))))

#|

(print (openai:completions "The President went to Congress" 20))

|#
