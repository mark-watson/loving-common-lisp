;;; embeddings.lisp — Gemini embedding integration
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(in-package #:rag)

;;; Uses the Gemini gemini-embedding-001 model for computing document
;;; and query embeddings (text-embedding-004 was retired from the
;;; v1beta API). This model is inexpensive and available on the free tier.
;;; HTTP goes through llm:post-json (Dexador), the same path as the
;;; rest of the llm library; Dexador signals dex:http-request-failed
;;; on 4xx/5xx responses.

;;; ---- Verbosity control (loaded first; used by all other files) ----

(defvar *rag-verbose* t
  "When true (the default), the pipeline prints DEBUG tracing showing
   each agent's decisions. Bind or set to NIL for quiet library use.")

(defmacro %debug-log (control &rest args)
  "Like (format t CONTROL ARGS...) but suppressed when *rag-verbose* is NIL."
  `(when *rag-verbose*
     (format t ,control ,@args)))

;;; ---- Retry helper ----

(defun call-with-retries (thunk &key (attempts 3) (initial-delay 1.0))
  "Call THUNK, retrying on transient HTTP failures with exponential
   backoff (1s, 2s, 4s by default). Signals an error after ATTEMPTS
   failures, including the underlying condition."
  (loop for attempt from 1
        for delay = initial-delay then (* delay 2)
        do (handler-case (return (funcall thunk))
             (dex:http-request-failed (e)
               (when (>= attempt attempts)
                 (error "Gemini API request failed after ~A attempts: ~A"
                        attempts e))
               (%debug-log "~%DEBUG call-with-retries: attempt ~A/~A failed ~
                            (~A), retrying in ~A seconds~%"
                           attempt attempts e delay)
               (sleep delay)))))

;;; ---- Configuration ----

(defvar *embedding-model* "gemini-embedding-001")
(defvar *embedding-api-url*
  "https://generativelanguage.googleapis.com/v1beta/models/")

(defun get-google-api-key ()
  (or (uiop:getenv "GOOGLE_API_KEY")
      (error "GOOGLE_API_KEY environment variable is not set")))

;;; ---- Embedding cache ----

(defvar *embedding-cache* (make-hash-table :test 'equal)
  "Memoizes embeddings so re-running demos or tests does not re-embed
   previously seen text. Keyed on (model . text).")

(defun clear-embedding-cache ()
  (clrhash *embedding-cache*))

(defun embedding-cache-key (text)
  (cons *embedding-model* text))

;;; ---- Low-level API calls (with error checking and retries) ----

(defun %decode-embedding-response (response-string)
  "Decode an embedContent response, checking for API errors."
  (let* ((decoded (cl-json:decode-json-from-string response-string))
         (error-obj (cdr (assoc :ERROR decoded))))
    (when error-obj
      (error "Gemini embedding API error: ~A" response-string))
    (let* ((embedding-obj (cdr (assoc :EMBEDDING decoded)))
           (values-list (cdr (assoc :VALUES embedding-obj))))
      (unless values-list
        (error "Gemini embedding response contained no embedding vector: ~A"
               response-string))
      values-list)))

(defun %fetch-embedding (text)
  "Compute an embedding vector for TEXT via the embedContent endpoint.
   Returns a list of floats. Retries transient failures."
  (let* ((api-url (concatenate 'string
                               *embedding-api-url*
                               *embedding-model*
                               ":embedContent"
                               "?key=" (get-google-api-key)))
         (payload (make-hash-table :test 'equal)))
    (let ((content-ht (make-hash-table :test 'equal))
          (part-ht (make-hash-table :test 'equal)))
      (setf (gethash "text" part-ht) text)
      (setf (gethash "parts" content-ht) (list part-ht))
      (setf (gethash "content" payload) content-ht)
      (setf (gethash "model" payload)
            (concatenate 'string "models/" *embedding-model*)))
    (%decode-embedding-response
     (call-with-retries
      (lambda ()
        (llm:post-json api-url
                       (list '("Content-Type" . "application/json"))
                       payload))))))

(defun %fetch-embeddings-batch (texts)
  "Compute embeddings for all TEXTS in a single batchEmbedContents call.
   Returns a list of embedding vectors in the same order as TEXTS."
  (let* ((api-url (concatenate 'string
                               *embedding-api-url*
                               *embedding-model*
                               ":batchEmbedContents"
                               "?key=" (get-google-api-key)))
         (payload (make-hash-table :test 'equal)))
    (setf (gethash "requests" payload)
          (mapcar (lambda (text)
                    (let ((request (make-hash-table :test 'equal))
                          (content-ht (make-hash-table :test 'equal))
                          (part-ht (make-hash-table :test 'equal)))
                      (setf (gethash "text" part-ht) text
                            (gethash "parts" content-ht) (list part-ht)
                            (gethash "content" request) content-ht
                            (gethash "model" request)
                            (concatenate 'string "models/" *embedding-model*))
                      request))
                  texts))
    (let* ((response-string
             (call-with-retries
              (lambda ()
                (llm:post-json api-url
                               (list '("Content-Type" . "application/json"))
                               payload))))
           (decoded (cl-json:decode-json-from-string response-string))
           (error-obj (cdr (assoc :ERROR decoded)))
           (embeddings (cdr (assoc :EMBEDDINGS decoded))))
      (when error-obj
        (error "Gemini batch embedding API error: ~A" response-string))
      (unless (and embeddings (= (length embeddings) (length texts)))
        (error "batchEmbedContents returned ~A embeddings for ~A texts: ~A"
               (length embeddings) (length texts) response-string))
      (mapcar (lambda (embedding-obj)
                (or (cdr (assoc :VALUES embedding-obj))
                    (error "Embedding without values: ~A" response-string)))
              embeddings))))

;;; ---- Public embedding interface (injectable for tests) ----

(defvar *embedding-fn* #'%fetch-embedding
  "Function of one argument (a string) returning an embedding vector.
   Rebind this in tests to run the pipeline without network access.")

(defun get-embedding (text)
  "Compute (or retrieve from cache) an embedding vector for TEXT.
   Returns a list of floats."
  (let ((key (embedding-cache-key text)))
    (multiple-value-bind (cached present-p) (gethash key *embedding-cache*)
      (if present-p
          (progn
            (%debug-log "~%DEBUG get-embedding: cache hit for ~S~%"
                        (subseq text 0 (min 60 (length text))))
            cached)
          (let ((vec (funcall *embedding-fn* text)))
            (setf (gethash key *embedding-cache*) vec)
            (%debug-log "~%DEBUG get-embedding: got ~A-dimensional vector for ~S~%"
                        (length vec) (subseq text 0 (min 60 (length text))))
            vec)))))

(defun get-embeddings (texts)
  "Compute embeddings for a list of TEXTS. When the default embedding
   function is in use, all cache misses are fetched with a single
   batchEmbedContents API call (one HTTP request instead of N)."
  (when (eq *embedding-fn* #'%fetch-embedding)
    (let ((misses (remove-duplicates
                   (remove-if (lambda (text)
                                (nth-value 1 (gethash (embedding-cache-key text)
                                                      *embedding-cache*)))
                              texts)
                   :test #'equal)))
      (when misses
        (%debug-log "~%DEBUG get-embeddings: batch-fetching ~A embeddings~%"
                    (length misses))
        (loop for text in misses
              for vec in (%fetch-embeddings-batch misses)
              do (setf (gethash (embedding-cache-key text) *embedding-cache*)
                       vec)))))
  (mapcar #'get-embedding texts))

;;; ---- Vector math ----

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
