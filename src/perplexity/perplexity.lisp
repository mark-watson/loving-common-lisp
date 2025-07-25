(in-package #:perplexity)

;; define the environment variable "PERPLEXITY_API_KEY" with the value of your Perplexity API key

(defvar *model-host* "https://api.perplexity.ai/chat/completions")
(defvar *model* "sonar-pro")

(defun research-helper (curl-command)
  "this function is identical to the function openai-helper in
   the library https://github.com/mark-watson/openai"
  (princ curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (pprint response)
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        ;; extract text (this might change if Perplexity or OpenAI changes JSON return format):
        (cdr (assoc :content (cdr (assoc :message (cadr (assoc :choices json-as-list))))))))))


(defun research (starter-text)
  "Send a search and LLM request"
  (let* ((input-text (write-to-string starter-text))
         (request-body
          (cl-json:encode-json-to-string
           `((:messages . (((:role . "user") (:content . ,input-text))))
             (:model . ,*model*))))
         (curl-command
           (format nil 
                  "curl ~A ~
                   -H \"Content-Type: application/json\" ~
                   -H \"Authorization: Bearer ~A\" ~
                   -d '~A'"
                   *model-host*
                   (uiop:getenv "PERPLEXITY_API_KEY")
                   request-body)))
    (research-helper curl-command)))

#|

(print (perplexity:research "consultant Mark Watson has written AI and Lisp books. What musical instruments does he play?"))

|#
