(in-package #:openai)


(defun summarize (some-text max-tokens)
  (let ((curl-command
         (concatenate 'string
                      "curl " *model-host*
                      " -H \"Content-Type: application/json\""
                      " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                      " -d '{\"messages\": [{\"role\": \"user\", \"content\": \"Summarize: " some-text 
                      "\"}], \"model\": \"gpt-4\", \"max_tokens\": " (write-to-string max-tokens) "}'")))
    (openai-helper curl-command)))

(defun answer-question (question-text max-tokens)
  (completions question-text max-tokens))

(defun embeddings (text)
  "Get embeddings using text-embedding-3-small model (1536 dimensions)"
  (let* ((curl-command
          (concatenate 'string
                       "curl https://api.openai.com/v1/embeddings "
                       " -H \"Content-Type: application/json\""
                       " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                       " -d '{\"input\": \"" text 
                       "\", \"model\": \"text-embedding-3-small\"}'"))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))

(defun dot-product-recursive (a b)
  "Calculate dot product recursively"
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product-recursive (rest a) (rest b)))))

(defun dot-product (list1 list2)
  "Calculate dot product iteratively"
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do (setf sum (+ sum (* x y))))
    sum))