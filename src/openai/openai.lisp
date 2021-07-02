(in-package #:openai)

;;(print (uiop:getenv "OPENAI_KEY")) ;; define this environment variable with the value of your OpenAI API key

(defun completions (starter-text &aux (host "https://api.openai.com/v1/engines/davinci/completions"))
  (let* ((curl-command
	  (concatenate
	   'string
	   "curl " host
	   " -H \"Content-Type: application/json\""
	   " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" " 
	   host
	   " -d '{\"prompt\": \"" starter-text "\", \"max_tokens\": 15}'"))
	 (ignore_1 (let () (terpri) (princ curl-command) (terpri)))
	 (response
	  (uiop:run-program
	   curl-command
	    :output :string)))
    (print response)
      (with-input-from-string
          (s response)
	(let* ((json-as-list (json:decode-json s)))
	  (print json-as-list)))))

#|

(print (completions "The President went to Congress"))

|#
