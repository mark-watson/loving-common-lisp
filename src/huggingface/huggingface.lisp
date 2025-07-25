(in-package #:huggingface)

;; define the environment variable "HF_API_TOKEN" with the value of your Hugging Face API key

(defvar huggingface-davinci-model-host "https://api.huggingface.com/v1/engines/davinci/completions")

(defun huggingface-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        json-as-list))))

(defun summarize (some-text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl https://api-inference.huggingface.co/models/facebook/bart-large-cnn"
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "HF_API_TOKEN") "\" " 
           " -d '{\"inputs\": \"" some-text "\", \"max_length\": "
           (write-to-string max-tokens) " }'")))
    (cdaar (huggingface-helper curl-command))))

(defun answer-question (question-text context-text)
  (let* ((curl-command
          (concatenate
           'string
           "curl https://api-inference.huggingface.co/models/deepset/roberta-base-squad2"
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "HF_API_TOKEN") "\" " 
           " -d '{\"question\": \"" question-text "\", \"context\": \""
           context-text "\" }'"))
         (answer (huggingface-helper curl-command)))
    (cdar (last answer))))

#|
(huggingface:summarize "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus." 30)
(huggingface:answer-question "Where were the 1992 Olympics held?" "The 1992 Olympics were in Greece")
|#
