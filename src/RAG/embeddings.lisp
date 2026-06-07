;;; embeddings.lisp — Gemini text-embedding-004 integration
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(in-package #:rag)

;;; Uses the Gemini text-embedding-004 model for computing document
;;; and query embeddings. This model is inexpensive and available on
;;; the free tier.

(defvar *embedding-model* "text-embedding-004")
(defvar *embedding-api-url*
  "https://generativelanguage.googleapis.com/v1beta/models/")

(defun get-google-api-key ()
  (or (uiop:getenv "GOOGLE_API_KEY")
      (error "GOOGLE_API_KEY environment variable is not set")))

(defun get-embedding (text)
  "Compute an embedding vector for TEXT using Gemini text-embedding-004.
   Returns a list of floats."
  (let* ((api-url (concatenate 'string
                               *embedding-api-url*
                               *embedding-model*
                               ":embedContent"
                               "?key=" (get-google-api-key)))
         (payload (make-hash-table :test 'equal)))
    ;; Build the request payload
    (let ((content-ht (make-hash-table :test 'equal))
          (part-ht (make-hash-table :test 'equal)))
      (setf (gethash "text" part-ht) text)
      (setf (gethash "parts" content-ht) (list part-ht))
      (setf (gethash "content" payload) content-ht)
      (setf (gethash "model" payload)
            (concatenate 'string "models/" *embedding-model*)))
    (let* ((payload-json (cl-json:encode-json-to-string payload))
           (curl-command
             (list "curl" "-s" "-X" "POST"
                   "-H" "Content-Type: application/json"
                   "-d" payload-json
                   api-url))
           (response-string
             (uiop:run-program curl-command
                               :output :string
                               :error-output :string
                               :ignore-error-status t))
           (decoded (cl-json:decode-json-from-string response-string))
           (embedding-obj (cdr (assoc :EMBEDDING decoded)))
           (values-list (cdr (assoc :VALUES embedding-obj))))
      (format t "~%DEBUG get-embedding: got ~A-dimensional vector for ~S~%"
              (length values-list)
              (subseq text 0 (min 60 (length text))))
      values-list)))

(defun dot-product (vec-a vec-b)
  "Compute the dot product of two numeric lists."
  (loop for a in vec-a
        for b in vec-b
        sum (* a b)))

(defun vector-magnitude (vec)
  "Compute the magnitude (L2 norm) of a numeric list."
  (sqrt (loop for x in vec sum (* x x))))

(defun cosine-similarity (vec-a vec-b)
  "Compute cosine similarity between two embedding vectors.
   Returns a value between -1 and 1."
  (let ((mag-a (vector-magnitude vec-a))
        (mag-b (vector-magnitude vec-b)))
    (if (or (zerop mag-a) (zerop mag-b))
        0.0
        (/ (dot-product vec-a vec-b) (* mag-a mag-b)))))
