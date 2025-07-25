(in-package #:anthropic)

;; define the environment variable "ANTHROPIC_API_KEY" with the value of your ANTHROPIC_API_KEY API key

(defvar anthropic-host "https://api.anthropic.com/v1/complete")

(defun anthropic-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        (string-trim
          " "
          (cdr (assoc :completion json-as-list)))))))

(defun completions (text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl --request POST --url " anthropic-host
           " -H \"accept: application/json\""
           " -H \"anthropic-version: 2023-06-01\""
           " -H \"content-type: application/json\""
           " -H \"x-api-key: " (uiop:getenv "ANTHROPIC_API_KEY") "\""
           " --data '{ \"prompt\": \"\\n\\nHuman: "
           text
           "\\n\\nAssistant: \", \"max_tokens_to_sample\": "
           (princ-to-string max-tokens)
           ", \"model\": \"claude-instant-1\" }'")))
    (anthropic-helper curl-command)))

#|

(print (anthropic:completions "Answer concisely. Mary is 30 years old and Bob is 25. Who is older?" 12))

|#
