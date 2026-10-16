(in-package #:mistral)

;; define the environment variable "MISTRAL_API_KEY" with the value of your mistral API key


(defvar *model-host* "https://api.mistral.ai/v1/chat/completions")
(defvar *default-model* "mistral-small-latest") ;; Latest small model as of 2026

(defun mistral-api-key ()
  (or (uiop:getenv "MISTRAL_API_KEY")
      (error "Set the MISTRAL_API_KEY environment variable")))

(defun curl-post-json (url json-data)
  "POST the JSON-DATA string to URL using curl and return the response string.
   curl is run with an argument list (no shell), so quotes in the data are safe."
  (uiop:run-program
   (list "curl" "-s" url
         "-H" "Content-Type: application/json"
         "-H" (concatenate 'string "Authorization: Bearer " (mistral-api-key))
         "-d" json-data)
   :output :string
   :error-output :string))

(defun mistral-helper (response)
  "Extract the message content from a chat completion RESPONSE string."
  (with-input-from-string
      (s response)
    (let* ((json-as-list (json:decode-json s)))
      ;; More robust JSON path traversal
      (cdr (assoc :content (cdr (assoc :message (car (cdr (assoc :choices json-as-list))))))))))

(defun completions (starter-text &optional (max-tokens 100) (model *default-model*))
  (let* ((d
          (cl-json:encode-json-to-string
           `((:messages . (((:role . "user") (:content . ,starter-text))))
             (:model . ,model)
             (:max_tokens . ,max-tokens)))))
    (mistral-helper (curl-post-json *model-host* d))))

(defun summarize (some-text &optional (max-tokens 100) (model *default-model*))
  (completions (concatenate 'string "Summarize: " some-text) max-tokens model))

(defun answer-question (question-text &optional (max-tokens 100) (model *default-model*))
  (let ((q-text
          (concatenate
           'string
           (string #\Newline) "Q: " question-text (string #\Newline) "A:")))
    (completions q-text max-tokens model)))


(defun embeddings (text &optional (model "mistral-embed"))
  (let ((data (cl-json:encode-json-to-string
               `((:input . ,(list text)) (:model . ,model)))))
    (let ((response (curl-post-json "https://api.mistral.ai/v1/embeddings" data)))
      (with-input-from-string
          (s response)
        (let ((json-as-list (json:decode-json s)))
          ;; return a list of embeddings (typically 1024 floats):
          (cdr (assoc :embedding (car (cdr (assoc :data json-as-list))))))))))

(defun dot-product (list1 list2)
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do
               (setf sum (+ sum (* x y))))
    sum))

;; (print (dot-product '(1 2 3) '(4 5 6)))
;; (print (mistral::embeddings "John bought a new car"))

#|

;; Example usage with the updated API:

;; Basic completion (max-tokens is now optional, defaults to 100)
(print (mistral:completions "The President went to Congress"))
(print (mistral:completions "The President went to Congress" 20)) ;; with explicit max-tokens

;; Summarization (max-tokens is now optional)
(print (mistral:summarize "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus."))

;; Question answering (max-tokens is now optional)
(print (mistral:answer-question "Where were the 1992 Olympics held?"))
(print (mistral:answer-question "Where is the Valley of Kings?"))
(print (mistral:answer-question "Mary is 30 years old and Bob is 25. Who is older?"))

;; Using different models (optional parameter)
(print (mistral:completions "Tell me about AI" 50 "mistral-tiny"))

;; Get embeddings
(print (mistral:embeddings "John bought a new car"))

|#
