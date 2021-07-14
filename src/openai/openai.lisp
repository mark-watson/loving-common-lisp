(in-package #:openai)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defvar openai-davinci-model-host "https://api.openai.com/v1/engines/davinci/completions")

(defun openai-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    ;;(princ curl-command)
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        ;; extract text (this might change if OpenAI changes JSON return format):
        (cdar (cadr (nth 4 json-as-list)))))))
  
(defun completions (starter-text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl " openai-davinci-model-host
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" " 
           " -d '{\"prompt\": \"" starter-text "\", \"max_tokens\": "
           (write-to-string max-tokens)  "}'")))
    (openai-helper curl-command)))

(defun summarize (some-text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl " openai-davinci-model-host
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" " 
           " -d '{\"prompt\": \"" some-text "\", \"max_tokens\": "
           (write-to-string max-tokens) ", \"presence_penalty\": 0.0"
           ", \"temperature\": 0.3, \"top_p\": 1.0, \"frequency_penalty\": 0.0 }'")))
    (openai-helper curl-command)))

(defun answer-question (question-text max-tokens)
  (let* ((q-text
          (concatenate
           'string
           "\nQ: " question-text "\nA:"))
         (curl-command
          (concatenate
           'string
           "curl " openai-davinci-model-host
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" " 
           " -d '{\"prompt\": \"" q-text "\", \"max_tokens\": "
           (write-to-string max-tokens) ", \"presence_penalty\": 0.0, \"stop\": [\"\\n\"]"
           ", \"temperature\": 0.0, \"top_p\": 1.0, \"frequency_penalty\": 0.0 }'"))
         (answer (openai-helper curl-command))
         (index (search "nQ:" answer)))
    (if index
        (string-trim " " (subseq answer 0 index))
        (string-trim " " answer))))

#|

(print (openai:completions "The President went to Congress" 20))

(print (openai:summarize "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus." 30))

(print (openai:answer-question "Where were the 1992 Olympics held?" 60))
(print (openai:answer-question "Where is the Valley of Kings?" 60))
|#
