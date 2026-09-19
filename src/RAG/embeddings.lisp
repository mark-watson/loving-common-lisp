;;; embeddings.lisp — Embedding integration via litelm
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(in-package #:rag)

;;; Embeddings are computed through the litelm routing library: the
;;; "gemini/gemini-embedding-001" model string routes to Google's
;;; OpenAI-compatible /embeddings endpoint, and litelm handles the JSON and
;;; HTTP details -- including mapping HTTP failures onto its condition
;;; hierarchy (litelm:rate-limit-error, litelm:api-error, ...).
;;; The API key comes from GEMINI_API_KEY or GOOGLE_API_KEY.

;;; ---- Verbosity control (loaded first; used by all other files) ----

(defvar *rag-verbose* t
  "When true (the default), the pipeline prints DEBUG tracing showing
    each agent's decisions. Bind or set to NIL for quiet library use.")

(defmacro %debug-log (control &rest args)
  "Like (format t CONTROL ARGS...) but suppressed when *rag-verbose* is NIL."
  `(when *rag-verbose*
     (format t ,control ,@args)))

;;; ---- Retry helper ----

(defun %transient-p (condition)
  "True when CONDITION is worth retrying: HTTP 429/5xx (which litelm
    reports as rate-limit-error / api-error), or a connection-level
    failure (usocket). Permanent 4xx client errors (bad request, bad API
    key, wrong model name) signal immediately."
  (typecase condition
    (litelm:rate-limit-error t)                     ; HTTP 429
    (litelm:api-error (>= (litelm:api-error-status condition) 500))
    (usocket:socket-error t)
    (usocket:ns-error t)
    (t nil)))

(defparameter *retry-sleep-fn* #'sleep
  "Function called to pause between retries. Rebind to a no-op in
    tests so retry backoff does not slow the suite down.")

(defun call-with-retries (thunk &key (attempts 3) (initial-delay 1.0))
  "Call THUNK, retrying transient failures (HTTP 429/5xx, connection
    errors) with exponential backoff (1s, 2s, 4s by default). Permanent
    HTTP 4xx errors signal immediately; after ATTEMPTS transient
    failures an error is signaled, including the underlying condition."
  (loop for attempt from 1
        for delay = initial-delay then (* delay 2)
        do (handler-case (return (funcall thunk))
             (error (e)
               (cond ((not (%transient-p e))
                      (error "Non-transient API error: ~A" e))
                     ((>= attempt attempts)
                      (error "API request failed after ~A attempts: ~A"
                             attempts e))
                     (t
                      (%debug-log "~%DEBUG call-with-retries: attempt ~A/~A failed ~
                                   (~A), retrying in ~A seconds~%"
                                  attempt attempts e delay)
                      (funcall *retry-sleep-fn* delay)))))))

;;; ---- Configuration ----

(defparameter *embedding-model* "gemini/gemini-embedding-001"
  "Embedding model as a litelm \"provider/model\" string. If you change
    this you must re-embed existing corpora: saved corpus files hold
    vectors from the old model/dimension and search signals a dimension
    mismatch.")

(defparameter *embedding-dimension* nil
  "Output embedding dimension, or NIL for the model default (3072 for
    gemini-embedding-001). Providers accept 768, 1536, or 3072 for this
    model; 768 saves 4x memory and search time with little quality
    loss. Set before building or loading a corpus. Passed to litelm as
    the OpenAI-compatible \"dimensions\" parameter.")

(defparameter *embedding-batch-limit* 100
  "Maximum texts per embeddings request; the API rejects more.")

(defparameter *api-base* nil
  "Optional override for the provider's base URL, passed straight to
    litelm. NIL uses the provider default. Useful for a proxy or a
    local test server.")

;;; ---- Embedding cache ----

(defparameter *embedding-cache-cap* 50000
  "Maximum entries kept in *embedding-cache*. When the cap is reached
    the cache is cleared and refilled. NIL means never evict.")

(defvar *embedding-cache* (make-hash-table :test 'equal)
  "Memoizes embeddings so re-running demos or tests does not re-embed
    previously seen text. Keyed on (model . text). See
    *embedding-cache-cap* for eviction.")

(defun embedding-cache-key (text)
  (cons *embedding-model* text))

(defun clear-embedding-cache ()
  (clrhash *embedding-cache*))

(defun %cache-put (text vec)
  "Store VEC for TEXT, evicting the whole cache when the cap is reached
    (simple and safe; a full rebuild costs a few batched API calls)."
  (when (and *embedding-cache-cap*
             (>= (hash-table-count *embedding-cache*) *embedding-cache-cap*))
    (%debug-log "~%DEBUG embedding cache reached ~A entries; clearing~%"
                *embedding-cache-cap*)
    (clrhash *embedding-cache*))
  (setf (gethash (embedding-cache-key text) *embedding-cache*) vec))

;;; ---- Low-level API calls (with error checking and retries) ----

(defun %check-embedding-vectors (vectors expected)
  "Coerce VECTORS (the list of vectors litelm returned) into simple-vectors,
    checking that the provider returned one non-empty vector per input text."
  (unless (= (length vectors) expected)
    (error "Embedding request returned ~A vectors for ~A texts"
           (length vectors) expected))
  (mapcar (lambda (vector)
            (unless (and (consp vector) (every #'numberp vector))
              (error "Embedding response contained no vector values: ~S"
                     vector))
            (coerce vector 'simple-vector))
          vectors))

(defun %fetch-embedding (text)
  "Compute an embedding vector for TEXT through litelm. Returns a
    simple-vector of floats. Retries transient failures."
  (first (%check-embedding-vectors
          (call-with-retries
           (lambda ()
             (litelm:embedding *embedding-model* text
                               :dimensions *embedding-dimension*
                               :api-base *api-base*)))
          1)))

(defun %post-batch-request (texts)
  "Request embeddings for TEXTS (at most *embedding-batch-limit* of them)
    in a single litelm call and return the vectors in the same order."
  (%check-embedding-vectors
   (call-with-retries
    (lambda ()
      (litelm:embedding *embedding-model* texts
                        :dimensions *embedding-dimension*
                        :api-base *api-base*)))
   (length texts)))

(defparameter *batch-request-fn* #'%post-batch-request
  "Function of one argument (a list of texts, at most
    *embedding-batch-limit* long) returning a list of embedding vectors
    in the same order. Rebind in tests to stub the HTTP layer.")

(defun %fetch-embeddings-batch (texts)
  "Compute embeddings for all TEXTS, splitting into batches of at most
    *embedding-batch-limit* texts per litelm request (the API cap).
    Returns a list of vectors in the same order as TEXTS."
  (loop for batch on texts by (lambda (l) (nthcdr *embedding-batch-limit* l))
        nconc (funcall *batch-request-fn*
                       (subseq batch 0
                               (min *embedding-batch-limit* (length batch))))))

;;; ---- Public embedding interface (injectable for tests) ----

(defparameter *embedding-fn* #'%fetch-embedding
  "Function of one argument (a string) returning an embedding vector.
    Rebind this in tests to run the pipeline without network access.
    When left at its default, GET-EMBEDDINGS batches all cache misses
    through *batch-request-fn* instead.")

(defun get-embedding (text)
  "Compute (or retrieve from cache) an embedding vector for TEXT.
    Returns a simple-vector of floats."
  (let ((key (embedding-cache-key text)))
    (multiple-value-bind (cached present-p) (gethash key *embedding-cache*)
      (if present-p
          (progn
            (%debug-log "~%DEBUG get-embedding: cache hit for ~S~%"
                        (subseq text 0 (min 60 (length text))))
            cached)
          (let ((vec (coerce (funcall *embedding-fn* text) 'simple-vector)))
            (%cache-put text vec)
            (%debug-log "~%DEBUG get-embedding: got ~A-dimensional vector for ~S~%"
                        (length vec) (subseq text 0 (min 60 (length text))))
            vec)))))

(defun get-embeddings (texts)
  "Compute embeddings for a list of TEXTS. When the default embedding
    function is in use, all cache misses are fetched with batched
    batched litelm calls (at most *embedding-batch-limit* texts
    per request) instead of one HTTP round trip per text."
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
              do (%cache-put text vec)))))
  (mapcar #'get-embedding texts))

;;; ---- Vector math ----

(defun dot-product (vec-a vec-b)
  "Compute the dot product of two equal-length vectors of floats.
    Signals an error on length mismatch: silently truncating would hide
    a model/dimension change and corrupt similarity scores."
  (let ((a (coerce vec-a 'simple-vector))
        (b (coerce vec-b 'simple-vector)))
    (unless (= (length a) (length b))
      (error "Embedding dimension mismatch: ~A vs ~A (did *embedding-model* ~
              or *embedding-dimension* change after the corpus was built?)"
             (length a) (length b)))
    (loop for x across a
          for y across b
          sum (* x y))))

(defun vector-magnitude (vec)
  "Compute the magnitude (L2 norm) of a vector of floats."
  (let ((v (coerce vec 'simple-vector)))
    (sqrt (loop for x across v sum (* x x)))))

(defun cosine-similarity (vec-a vec-b)
  "Compute cosine similarity between two embedding vectors.
    Returns a value between -1 and 1."
  (let ((mag-a (vector-magnitude vec-a))
        (mag-b (vector-magnitude vec-b)))
    (if (or (zerop mag-a) (zerop mag-b))
        0.0
        (/ (dot-product vec-a vec-b) (* mag-a mag-b)))))

(defun normalize-vector (vec)
  "Return VEC scaled to unit length (as a simple-vector). Normalized
    vectors make cosine similarity a plain dot product, so chunk norms
    are computed once at add time instead of per query. Vectors whose
    magnitude is already 1 within single-float precision are returned
    unchanged so normalization stays idempotent."
  (let* ((v (coerce vec 'simple-vector))
         (mag (vector-magnitude v)))
    (cond ((or (zerop mag) (<= (abs (- 1.0f0 mag)) 1.0e-6))
           v)
          (t
           (map 'simple-vector (lambda (x) (/ x mag)) v)))))