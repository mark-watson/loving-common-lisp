(in-package #:openai)


(defun summarize (some-text)
  (let* ((data `((messages . (((role . "user") (content . ,(concatenate 'string "Summarize: " some-text)))))
                 (model . "gpt-5-nano")))
         (response-str (openai-post *model-host* data)))
    (openai-helper response-str)))

(defun answer-question (question)
  (completions (concatenate 'string "Concisely answer the question: " question)))

(defun embeddings (text)
  "Get embeddings using text-embedding-3-small model (1536 dimensions)"
  (let* ((payload `((input . ,text)
                    (model . "text-embedding-3-small")))
         (response-str (openai-post "https://api.openai.com/v1/embeddings" payload))
         (json-as-list (cl-json:decode-json-from-string response-str))
         (data (cdr (assoc :data json-as-list)))
         (first-item (car data))
         (emb (cdr (assoc :embedding first-item))))
    emb))

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
