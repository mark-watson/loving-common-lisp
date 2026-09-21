# Agentic RAG Using the Gemini LLM APIs

Dear reader, the previous chapter showed a practical RAG implementation that is easy to understand and incorporate in your own projects. Here we develop a more complex RAG system.
This chapter implements an **Agentic Retrieval-Augmented Generation (RAG)** system in Common Lisp, inspired by Google's June 2026 research blog post [Unlocking Dependable Responses with Agentic RAG](https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/).

In the previous chapter on document question answering, we built a "vanilla" RAG system: embed documents, embed the query, find similar chunks, and pass them to an LLM for answer generation. That approach works well for simple factual questions, but falls short on complex queries that require information from multiple sources or where the first retrieval pass misses critical details.

Agentic RAG addresses this limitation by introducing multiple specialized agents that **plan, rewrite queries, assess context sufficiency, and iteratively search** until enough information is gathered to produce a reliable answer. The key insight from the Google research is the **Sufficient Context Agent**, a quality-control step that evaluates whether the retrieved passages actually contain enough information to answer the question, and if not, generates specific feedback about what's missing so the system can refine its search.

The source code for this example is in the directory **src/RAG** of the book's GitHub repository. It uses the Gemini `gemini-embedding-001` model for embeddings (free tier) and `gemini-3-flash-preview` for all agent LLM calls (very inexpensive).

## Overview of the Agentic RAG Architecture

The system implements a multi-agent pipeline with five phases:

1. **Query Rewriting**: A Gemini-powered agent decomposes complex questions into 1–3 focused sub-queries for retrieval.
2. **Search Fanout**: All sub-queries are embedded in one batched API call, then each is searched across multiple document corpora. Results are deduplicated.
3. **Sufficient Context Assessment**: A specialized agent evaluates whether the retrieved passages contain enough information to fully answer the original question.
4. **Iterative Refinement**: If context is insufficient, the system generates refined search queries based on feedback about what's missing, then searches again. This loop repeats up to a configurable limit.
5. **Synthesis**: Once context is sufficient (or the iteration limit is reached), a synthesis agent generates a grounded answer citing source documents.

This differs fundamentally from vanilla RAG. In a vanilla system, if the first retrieval doesn't find the right passages, you get a partial answer or a hallucination. In agentic RAG, the system recognizes the gap and actively searches for the missing information.

## Project Structure

The project is organized as an ASDF system with five source files, a test file, and sample data:

| File | Description |
|---|---|
| **embeddings.lisp** | Gemini embedding integration (batch API, caching, retries) and cosine similarity |
| **vector-store.lisp** | In-memory vector store with document chunking and corpus persistence |
| **agents.lisp** | Multi-agent pipeline (rewriter, search, sufficiency, synthesis) |
| **rag.lisp** | Top-level API, interactive demo, and test code |
| **tests.lisp** | Offline unit tests that run without an API key |
| **data/** | Sample text documents for the demo |

The ASDF system definition in **rag.asd** defines both the library and its offline test system:

```lisp
(asdf:defsystem #:rag
  :description "Agentic RAG (Retrieval-Augmented Generation) using Gemini via the litelm routing library"
  :author "Mark Watson"
  :license "Apache 2"
  :version "1.1.0"
  :serial t
  :depends-on (#:litelm #:usocket #:uiop)
  :components ((:file "package")
               (:file "embeddings")
               (:file "vector-store")
               (:file "agents")
               (:file "rag"))
  :in-order-to ((asdf:test-op (asdf:test-op #:rag/test))))

(asdf:defsystem #:rag/test
  :description "Offline unit tests for the rag system (no network access)."
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:rag #:uiop)
  :serial t
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :rag-tests :run-tests)))
```

The **usocket** dependency exists so the retry logic can recognize connection-level errors (refused connections, timeouts, DNS failures), which Dexador signals as usocket conditions rather than HTTP errors.

The package exports the main entry points:

```lisp
(defpackage #:rag
  (:use #:cl)
  (:export #:make-corpus
           #:add-document
           #:save-corpus
           #:load-corpus
           #:corpus-chunk-count
           #:query
           #:agentic-rag
           #:interactive-demo
           #:test
           #:*rag-verbose*
           #:*rag-model*
           #:*embedding-model*
           #:*embedding-dimension*
           #:*embedding-batch-limit*
           #:*embedding-cache-cap*
           #:clear-embedding-cache
           #:cosine-similarity
           #:dot-product
           #:vector-magnitude
           #:normalize-vector))
```

## Computing Embeddings With the Gemini API

The file **embeddings.lisp** provides the foundation for semantic search. We use Google's `gemini-embedding-001` model, which produces 3072-dimensional vectors and is available on the free tier.

All HTTP and JSON handling is delegated to the **litelm** routing library. The model is named as a litelm `"provider/model"` string — `"gemini/gemini-embedding-001"` — and litelm routes it to Google's OpenAI-compatible `/embeddings` endpoint, attaching the API key from `GEMINI_API_KEY` (or `GOOGLE_API_KEY`) as a bearer token and decoding the reply. This system contains no HTTP code of its own.

Because litelm owns the transport, it also owns error classification: an HTTP failure arrives as a condition in its hierarchy — `litelm:rate-limit-error` for 429, `litelm:authentication-error` for 401/403, `litelm:api-error` for everything else, with the status readable through `litelm:api-error-status`. Connection-level failures still surface as usocket conditions. Not every failure deserves a retry, so a small predicate classifies them:

```lisp
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
```

A 400 (malformed request) or 401 (bad API key) will fail again in exactly the same way, so retrying just burns seconds of backoff before surfacing the real problem. Status 429 (rate limited) and 5xx (server trouble) usually clear up, and connection errors usually mean a transient network problem:

```lisp
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
```

The **\*retry-sleep-fn\*** variable follows the same rebindable-function idiom we use for the API calls themselves: the default waits for real backoff, and the test suite rebinds it to a no-op so retry tests run instantly.

Debug output throughout the system goes through the **%debug-log** macro, which prints only when **\*rag-verbose\*** is true. The verbose tracing is valuable when following the pipeline in this chapter, and setting the variable to NIL turns the system into a quiet library.

Embeddings are memoized in **\*embedding-cache\***, a hash table keyed on the model name and text, so reloading a document or re-running the demo never pays for the same API call twice. Because a long-running process embedding many queries would grow the cache without bound, **%cache-put** clears it when it reaches **\*embedding-cache-cap\*** (50,000 entries by default):

```lisp
(defun %cache-put (text vec)
  "Store VEC for TEXT, evicting the whole cache when the cap is reached
    (simple and safe; a full rebuild costs a few batched API calls)."
  (when (and *embedding-cache-cap*
             (>= (hash-table-count *embedding-cache*) *embedding-cache-cap*))
    (%debug-log "~%DEBUG embedding cache reached ~A entries; clearing~%"
                *embedding-cache-cap*)
    (clrhash *embedding-cache*))
  (setf (gethash (embedding-cache-key text) *embedding-cache*) vec))
```

The model, its output dimension, and the batch size are configurable:

```lisp
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
```

The **\*embedding-dimension\*** knob is worth knowing about. The model supports Matryoshka output: you can truncate the 3072-value vector to 768 or 1536 values and keep most of the retrieval quality, at a quarter (or half) of the memory and search cost. The value travels to litelm as the OpenAI-compatible `dimensions` parameter. If a provider ignores it, the dimension check in the vector store catches the mismatch at search time rather than silently comparing vectors of different widths. **\*api-base\*** exists for the same practical reason: it lets you point litelm at a proxy or a local test server without touching the rest of the system.

The low-level function **%fetch-embedding** asks litelm for a single string's vector, and **%check-embedding-vectors** validates what comes back before it enters the corpus:

```lisp
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
```

Note the special variable **\*embedding-fn\***: the public entry point **get-embedding** calls whatever function it holds. Defaulting it to the real HTTP implementation while allowing tests to rebind it to a stub is a simple Common Lisp idiom we will use again for the LLM calls, and it is what makes the offline unit tests possible.

The batch path has its own injection point. **%post-batch-request** hands a whole group of texts to litelm in one call — litelm turns the list into the JSON `input` array — and **%fetch-embeddings-batch** splits longer input into groups of at most **\*embedding-batch-limit\*** texts, calling **\*batch-request-fn\*** per group (the API rejects a 101st text in a single request, which we confirmed the hard way):

```lisp
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
```

Two public functions round out the interface. **get-embedding** checks the cache before calling the API, and **get-embeddings** fetches all cache misses for a list of texts with batched litelm calls, turning N HTTP requests into a handful when a document is first loaded:

```lisp
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
```

We also define **cosine-similarity** to compare two embedding vectors; this is how we determine which document chunks are most relevant to a query:

```lisp
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
```

The dimension check in **dot-product** earns its keep. `loop for x across a for y across b` stops at the shorter vector, so before this check a 3072-value chunk and a 1536-value query (say, after switching **\*embedding-dimension\***) would silently score against a truncated vector instead of failing. A wrong-but-plausible score is worse than an error because nobody notices it.

The embedding API returns a JSON response containing a list of floating-point values. The cosine similarity between two vectors measures how similar their directions are in the high-dimensional embedding space, regardless of magnitude. A similarity of 1.0 means the texts are semantically identical; 0.0 means they are unrelated.

## In-Memory Vector Store

The file **vector-store.lisp** implements a simple in-memory document store. Production systems would use a dedicated vector database like Pinecone or Chroma, but for a book example, an in-memory list with brute-force cosine similarity is clearer and requires zero setup.

We define two structs: **document-chunk** holds a piece of text with its source filename, embedding vector, and precomputed norm; and **corpus** is a named collection of chunks:

```lisp
(defstruct (document-chunk (:print-function %print-document-chunk))
  "A chunk of text with its source file, embedding vector (a normalized
    simple-vector), and precomputed L2 norm (1.0 for normalized chunks)."
  text
  source
  embedding
  (norm 1.0 :type float))
```

```lisp
(defstruct (corpus (:print-function %print-corpus))
  "A named collection of document chunks for retrieval."
  name
  description
  (chunks nil))
```

A struct with 3072 floats per chunk is painful to inspect at the REPL: printing a corpus dumps thousands of numbers per chunk and swamps the terminal. Both structs therefore install custom print functions. A chunk prints its text and source in full but only the first 10 embedding values:

```text
#<DOCUMENT-CHUNK :SOURCE "renewable-energy.txt" :TEXT "Renewable Energy
Sources and Technologies ..." :EMBEDDING #(3.2552084e-4 6.510417e-4
9.765625e-4 0.0013020834 0.0016276041 0.001953125 0.0022786458
0.0026041667 0.0029296875 0.0032552083 ...) [3072 dimensions]>
```

and a corpus prints just its name, description, and chunk count:

```text
(#<CORPUS :NAME "renewable-energy" :DESCRIPTION "Renewable energy sources
and technologies" :CHUNKS 9>
 #<CORPUS :NAME "electric-vehicles" :DESCRIPTION "Electric vehicle
technology and infrastructure" :CHUNKS 7>
 #<CORPUS :NAME "climate-science" :DESCRIPTION "Climate science and carbon
emissions" :CHUNKS 7>)
```

The printers emit unreadable `#<...>` forms on purpose. A readable `#S(...)` form with a truncated embedding could be `read` back into a program as a chunk whose vector is only 10 values long; the `#<` prefix makes clear the printed form is for humans. Chunks are still reachable through `document-chunk-embedding`, and **save-corpus** remains the way to write them to disk.

The **norm** slot and the custom printer live in the same struct for related reasons: both are about treating embeddings as opaque bulk data. Normalizing each chunk embedding once, when the document is added, means search never recomputes a chunk norm; cosine similarity against a normalized chunk is just a dot product divided by the query's norm. **make-document-chunk/embedded** is the constructor that does the work:

```lisp
(defun make-document-chunk/embedded (text source raw-embedding)
  "Build a document-chunk with a normalized simple-vector embedding and
    its precomputed norm. All chunks in the store are normalized, so
    search is a dot product (see search-corpus)."
  (let* ((vec (coerce raw-embedding 'simple-vector))
         (norm (vector-magnitude vec)))
    (when (zerop norm)
      (error "Zero-magnitude embedding for chunk from ~A; cannot normalize" source))
    (make-document-chunk :text text :source source :embedding vec :norm norm)))
```

Each chunk also gets a stable identity, used for deduplication later:

```lisp
(defun document-chunk-key (chunk)
  "Stable identity for a chunk across corpora: (source . text). The
    same text in different source files is a different chunk."
  (cons (document-chunk-source chunk) (document-chunk-text chunk)))
```

The function **split-into-chunks** breaks a long text into overlapping pieces of approximately 500 characters each, trying to break at sentence boundaries (periods or newlines) rather than cutting words in half:

```lisp
(defparameter *default-chunk-size* 500
  "Default size in characters for splitting documents into chunks.")

(defparameter *chunk-overlap* 50
  "Number of characters to overlap between adjacent chunks.")

(defun split-into-chunks (text &key (chunk-size *default-chunk-size*)
                                    (overlap *chunk-overlap*))
  "Split TEXT into overlapping chunks of approximately CHUNK-SIZE characters.
    Tries to break at sentence boundaries when possible."
  (let ((chunks nil)
        (len (length text))
        (start 0))
    (loop while (< start len)
          do (let* ((end (min (+ start chunk-size) len))
                    ;; Try to find a sentence boundary at or before END
                    ;; (searching the window up to 80 chars back from END)
                    (break-pos
                      (if (>= end len)
                          end
                          (or (position #\. text :start (max start (- end 80))
                                                 :end end :from-end t)
                              (position #\Newline text :start (max start (- end 80))
                                                      :end end :from-end t)
                              end)))
                    ;; Advance past the break character
                    (actual-end (if (< break-pos end)
                                    (1+ break-pos)
                                    end)))
               ;; Guarantee forward progress: if the break search left us
               ;; at or before START, fall back to a hard cut at CHUNK-SIZE.
               ;; Without this guard a chunk could be empty or START could
               ;; fail to advance (looping forever or dropping text).
               (when (<= actual-end start)
                 (setf actual-end (min (+ start chunk-size) len)))
               (let ((chunk (string-trim '(#\Space #\Newline #\Tab)
                                         (subseq text start actual-end))))
                 (when (> (length chunk) 0)
                   (push chunk chunks)))
               ;; Never move START backwards: the overlap backtrack must
               ;; not undo progress made by the forward-progress guard.
               (setf start (if (>= actual-end len)
                               len
                               (max (1+ start) (- actual-end overlap))))))
    (nreverse chunks)))
```

The overlap between chunks (defaulting to 50 characters) ensures that information at chunk boundaries is not lost; a sentence that spans two chunks will appear in both. Note the two progress guards: without them, a document whose only sentence break lands at or before the start of the current window could produce an empty chunk or move `start` backwards, looping forever. Loops that compute their next position from searched positions always need an explicit monotonic-progress check.

The function **add-document** reads a file, chunks it, and computes embeddings for all chunks at once. When the default embedding function is in use, **get-embeddings** makes batched API calls for the whole document instead of one request per chunk:

```lisp
(defun add-document (corpus filepath &key (chunk-size *default-chunk-size*))
  "Read a text file, split it into chunks, compute embeddings (in
    batched API calls when the default embedding function is used),
    and add the chunks to CORPUS. Returns the number of chunks added."
  (%debug-log "~%DEBUG add-document: loading ~A~%" filepath)
  (let* ((text (uiop:read-file-string filepath))
         (chunks (split-into-chunks text :chunk-size chunk-size))
         (source (file-namestring filepath)))
    (%debug-log "DEBUG add-document: split into ~A chunks~%" (length chunks))
    (setf (corpus-chunks corpus)
          (nconc (corpus-chunks corpus)
                 (loop for chunk-text in chunks
                       for embedding in (get-embeddings chunks)
                       collect (make-document-chunk/embedded chunk-text
                                                            source
                                                            embedding))))
    (%debug-log "DEBUG add-document: added ~A chunks from ~A~%"
                (length chunks) source)
    (length chunks)))
```

Because embedding a corpus costs API calls, **save-corpus** and **load-corpus** persist a corpus, embeddings included, as a plain s-expression file. Loading is the risky half: a truncated or corrupt file would otherwise produce chunks with NIL embeddings that fail far from the cause, or worse, score quietly wrong. So **load-corpus** validates the overall shape and every chunk before trusting it:

```lisp
(defun %valid-chunk-data-p (chunk-data)
  "True when one saved chunk plist has non-empty TEXT and SOURCE and an
    EMBEDDING that is a non-empty sequence of numbers."
  (and (consp chunk-data)
       (stringp (getf chunk-data :text))
       (plusp (length (getf chunk-data :text)))
       (stringp (getf chunk-data :source))
       (plusp (length (getf chunk-data :source)))
       (let ((emb (getf chunk-data :embedding)))
         (and (typep emb 'sequence)
              (plusp (length emb))
              (every #'numberp emb)))))

(defun load-corpus (pathname)
  "Load a corpus previously written by SAVE-CORPUS. Returns a corpus struct.
    Signals an error when the file is truncated, corrupt, or contains a
    chunk missing its text, source, or embedding."
  (with-open-file (in pathname :direction :input)
    (with-standard-io-syntax
      (let* ((*read-eval* nil) ; never evaluate while reading data files
             (data (read in)))
        (unless (and (consp data)
                     (getf data :name)
                     (listp (getf data :chunks))
                     (getf data :chunks))
          (error "Corrupt corpus file ~A: expected (:name ...) (:chunks ...)" pathname))
        (let ((corpus (make-corpus :name (getf data :name)
                                   :description (getf data :description))))
          (setf (corpus-chunks corpus)
                (mapcar (lambda (chunk-data)
                          (unless (%valid-chunk-data-p chunk-data)
                            (error "Corrupt chunk in corpus file ~A: ~S"
                                   pathname chunk-data))
                          ;; Re-normalize on load: files saved by older
                          ;; versions may hold un-normalized vectors.
                          (make-document-chunk/embedded
                           (getf chunk-data :text)
                           (getf chunk-data :source)
                           (getf chunk-data :embedding)))
                        (getf data :chunks)))
          corpus)))))
```

Note the `*read-eval*` binding when loading: `read` on an untrusted file must never be allowed to evaluate embedded forms.

The **search-corpus** and **search-corpora** functions find the top-K most similar chunks for a given query embedding:

```lisp
(defun score-chunks (chunks query-embedding &key (query-norm 1.0))
  "Score CHUNKS against QUERY-EMBEDDING. Chunks are stored normalized,
    so cosine similarity is the dot product divided by the query norm;
    each chunk's norm is not recomputed."
  (loop for chunk in chunks
        collect (cons (/ (dot-product query-embedding
                                      (document-chunk-embedding chunk))
                         query-norm)
                      chunk)))

(defun %top-k-by-score (scored-chunks top-k)
  "Return the TOP-K entries of SCORED-CHUNKS (sorted by descending car)
    using a single O(n) selection pass instead of a full O(n log n) sort."
  (let ((k (min top-k (length scored-chunks))))
    (when (plusp k)
      ;; Repeatedly extract the max: k passes, each O(n). Worst case
      ;; k = n is O(n^2), but k is small (3 by default), so this beats
      ;; sorting at demo scale and stays O(n) for constant k.
      (let ((remaining (copy-list scored-chunks))
            (result nil))
        (dotimes (i k)
          (let ((best (loop for entry in remaining
                            maximize (car entry))))
            (let ((winner (find best remaining :key #'car)))
              (push winner result)
              (setf remaining (remove winner remaining :count 1)))))
        (nreverse result)))))

(defun search-corpus (corpus query-embedding &key (top-k 3))
  "Search CORPUS for the TOP-K chunks most similar to QUERY-EMBEDDING.
    Returns a list of (score . document-chunk) pairs, sorted by
    descending similarity. The query embedding may be normalized or raw;
    its norm is computed once here."
  (%top-k-by-score (score-chunks (corpus-chunks corpus)
                                 (coerce query-embedding 'simple-vector)
                                 :query-norm (vector-magnitude
                                              (coerce query-embedding
                                                      'simple-vector)))
                   top-k))

(defun search-corpora (corpora query-embedding &key (top-k 3))
  "Search multiple CORPORA for the TOP-K most similar chunks overall.
    Returns a list of (score . document-chunk) pairs."
  (let* ((query (coerce query-embedding 'simple-vector))
         (query-norm (vector-magnitude query))
         (all-results
           (loop for corpus in corpora
                 nconc (%top-k-by-score
                        (score-chunks (corpus-chunks corpus)
                                      query
                                      :query-norm query-norm)
                        top-k))))
    (%top-k-by-score all-results top-k)))
```

Scoring exploits the normalization done at add time: with every chunk at unit length, the cosine similarity between a chunk and the query is the dot product divided by the query's norm, computed once per query instead of once per chunk. And instead of sorting all n scores to take the top 3, **%top-k-by-score** extracts the maximum k times, which is O(kn); with small k that beats an O(n log n) sort. Neither optimization matters at 23 chunks, but they keep the search loop harmless at thousands of chunks, and they fall out of the normalized representation naturally.

An important feature for agentic RAG is that **search-corpora** accepts a list of corpora, enabling cross-corpus retrieval. The Google research article emphasizes this capability: real-world knowledge is often spread across separate databases managed by different teams. Our system searches all corpora simultaneously and returns the best results regardless of source.

## The Multi-Agent Pipeline

The file **agents.lisp** is the heart of the system. Each "agent" is a function that calls Gemini with a specialized prompt. This is a practical and effective pattern: we don't need an external agent framework to implement agent behaviors, just well-crafted prompts and structured response parsing.

We use `gemini/gemini-3-flash-preview` for all agent calls. This model is very inexpensive while being capable enough for query rewriting, sufficiency assessment, and synthesis. The function **rag-generate** delegates to a local helper **%generate**, which hands the prompt to `litelm:completion` and takes the text content out of the returned response struct. The call goes through the special variable **\*generate-fn\*** so tests can substitute a stub (the same idiom as **\*embedding-fn\*** above). It also wraps the call in **call-with-retries**, so a transient 500 during assessment or synthesis does not throw away the whole pipeline's work:

```lisp
(defparameter *rag-model* "gemini/gemini-3-flash-preview"
  "Model used for all agent LLM calls, as a litelm \"provider/model\"
    string. Override per call with the :model keyword argument to
    agentic-rag.")

;;; ---- Text generation via litelm ----

(defun %generate (prompt model)
  "Generate text for PROMPT with MODEL through litelm. litelm routes the
    model string to the provider's OpenAI-compatible chat endpoint, sends
    the JSON, and decodes the reply; we return its text content."
  (litelm:response-content
   (litelm:completion model
                      :messages prompt
                      :api-base *api-base*)))

(defparameter *generate-fn*
  (lambda (prompt &key (model *rag-model*))
    (%generate prompt model))
  "Function of (prompt &key model) returning generated text. Defaults
    to a thin wrapper around %GENERATE, which calls litelm.
    Rebind this in tests to run the pipeline without network access.")

(defun rag-generate (prompt &key (model *rag-model*))
  "Call the LLM through *generate-fn* with retries on transient
    failures (HTTP 429/5xx, connection errors), so one flaky request
    does not throw away the whole pipeline's work. Signals an error if
    the model returns no text."
  (or (call-with-retries
       (lambda () (funcall *generate-fn* prompt :model model)))
      (error "LLM returned no text for prompt: ~A"
             (subseq prompt 0 (min 80 (length prompt))))))
```

### Agent 1: The Query Rewriter

The Query Rewriter takes a complex user question and decomposes it into 1–3 focused sub-queries. For example, the question "How does the carbon footprint of manufacturing EV batteries compare to the emissions saved by charging EVs from renewable energy?" would be split into sub-queries like:

- "carbon footprint of EV battery manufacturing"
- "emissions saved by charging electric vehicles from renewable energy"

This decomposition improves retrieval because each sub-query targets a specific fact that might appear in a different document or section.

Parsing the model's response deserves care. The prompt says "no numbering, bullets, or extra text", but models drift, and a first implementation that trimmed the characters `- * 1 2 3 .` off both ends of each line mangled legitimate queries: "2024 lithium battery prices" became "024 lithium battery prices", and "1.5 MW turbine output" became "5 MW turbine output". Queries are exactly the kind of text with leading digits and decimal points. The fix strips only a leading list prefix: an optional bullet character, or digits followed by `.` or `)` followed by whitespace. The whitespace test is what distinguishes "1. fourth query" from "1.5 MW output":

```lisp
(defun %strip-list-prefix (line)
  "Remove an optional markdown/numbered list prefix from LINE and the
    surrounding whitespace. Only leading list syntax is stripped:
    interior and trailing digits are part of the query (so
    \"2024 lithium prices\", \"1.5 MW output\", and \"75-100 kg\" survive
    intact, while \"4. fourth query\" and \"- bullet\" are cleaned)."
  (flet ((ws-p (c) (member c '(#\Space #\Tab #\Return #\Newline))))
    (let* ((len (length line))
           (i 0))
      ;; skip leading whitespace
      (loop while (and (< i len) (ws-p (char line i))) do (incf i))
      ;; skip an optional bullet character
      (when (and (< i len) (member (char line i) '(#\- #\* #\+)))
        (incf i)
        (loop while (and (< i len) (ws-p (char line i))) do (incf i)))
      ;; skip an optional numbering: digits followed by . or ) followed
      ;; by whitespace. "1.5 MW" fails the whitespace test, so it stays.
      (let ((j i))
        (loop while (and (< j len) (digit-char-p (char line j))) do (incf j))
        (when (and (> j i) (< j len)
                   (member (char line j) '(#\. #\)))
                   (< (1+ j) len)
                   (ws-p (char line (1+ j))))
          (setf i (1+ j))
          (loop while (and (< i len) (ws-p (char line i))) do (incf i))))
      (string-trim '(#\Space #\Tab #\Return #\Newline) (subseq line i)))))

(defun parse-query-lines (response)
  "Extract one query per line from a rewriter agent RESPONSE, dropping
    empty lines and list prefixes. Query text itself is untouched."
  (remove-if (lambda (s) (zerop (length s)))
             (mapcar #'%strip-list-prefix
                     (uiop:split-string (or response "")
                                        :separator '(#\Newline)))))

(defun rewrite-queries (user-query &key (model *rag-model*))
  "Decompose USER-QUERY into 1-3 focused sub-queries for retrieval.
    Returns a list of query strings. The original query is always
    appended as a fallback so the fanout always searches for what the
    user actually asked."
  (%debug-log "~%DEBUG rewrite-queries: decomposing query...~%")
  (let* ((prompt
           (format nil
                   "You are a search query rewriter for a RAG system. ~
                    Your job is to break a complex user question into ~
                    1-3 simple, focused search queries that will help ~
                    retrieve relevant information from a document collection.~%~
                    ~%Rules:~
                    ~%- Output ONLY the queries, one per line~
                    ~%- No numbering, bullets, or extra text~
                    ~%- Each query should target a specific fact or concept~
                    ~%- Keep queries concise (under 15 words each)~
                    ~%~%User question: ~A" user-query))
         (queries (parse-query-lines (rag-generate prompt :model model))))
    (%debug-log "DEBUG rewrite-queries: generated ~A sub-queries:~%~{  - ~A~%~}"
                (length queries) queries)
    (remove-duplicates (append queries (list user-query)) :test #'equal)))
```

Appending the original query is deliberate. The rewriter's sub-queries aim at the pieces of a question, but the user's exact phrasing often matches the document's phrasing best; searching it directly is one more vector in the fanout and costs one more row in the batch embedding call.

### Agent 2: Search Fanout

The Search Fanout agent executes the sub-queries against all corpora. All sub-query embeddings are fetched with one call to **get-embeddings** (one batched API request for the whole list) instead of one round trip per query. Results are deduplicated by **document-chunk-key** (source and text together), so the same passage matched by several queries appears once, but identical text from two different files stays as two results:

```lisp
(defun search-fanout (corpora sub-queries &key (top-k 3))
  "Execute embedding search across CORPORA for each sub-query.
    All sub-query embeddings are fetched with one batched API call.
    Returns a deduplicated list of (score . document-chunk) pairs,
    sorted by descending score. Chunks are deduplicated by
    (source . text) so identical text in different files stays distinct."
  (%debug-log "~%DEBUG search-fanout: searching ~A corpora with ~A queries~%"
              (length corpora) (length sub-queries))
  ;; One batched embedding call for all sub-queries instead of N round trips
  (let ((query-embeddings (get-embeddings sub-queries))
        (all-results nil)
        (seen-keys (make-hash-table :test 'equal)))
    (mapc (lambda (query query-embedding)
            (%debug-log "DEBUG search-fanout: searching with: ~S~%" query)
            (dolist (result (search-corpora corpora query-embedding
                                            :top-k top-k))
              (let ((key (document-chunk-key (cdr result))))
                (unless (gethash key seen-keys)
                  (setf (gethash key seen-keys) t)
                  (push result all-results)))))
          sub-queries query-embeddings)
    ;; Sort by score descending
    (let ((sorted (sort all-results #'> :key #'car)))
      (%debug-log "DEBUG search-fanout: found ~A unique chunks~%" (length sorted))
      sorted)))
```

### Agent 3: The Sufficient Context Agent

This is the key innovation from the Google research. After retrieval, the Sufficient Context Agent evaluates whether the passages actually contain enough information. It asks Gemini to produce a structured verdict: SUFFICIENT or INSUFFICIENT, with a reason and a description of what's missing.

The structured output format (VERDICT/REASON/MISSING) makes it straightforward to parse the LLM's response programmatically. The parsing is factored into its own function, **parse-verdict-response**, partly so the logic is unit-testable without an API key, and partly so the fallback policy lives in exactly one place: a verdict we cannot parse is treated as SUFFICIENT, because the iteration limit is the only other thing bounding API cost:

```lisp
(defun parse-verdict-response (response)
  "Parse a Sufficient Context Agent RESPONSE of the form:
      VERDICT: SUFFICIENT | INSUFFICIENT
      REASON: ...
      MISSING: ...
    Returns two values: SUFFICIENT-P and FEEDBACK (the MISSING text).
    An unparseable verdict is treated as SUFFICIENT — this bounds API
    cost because the iteration limit is the only other safeguard."
  (let* ((lines (uiop:split-string (or response "") :separator '(#\Newline)))
         (verdict-line (find-if (lambda (line)
                                  (search "VERDICT:" line :test #'char-equal))
                                lines))
         (missing-line (find-if (lambda (line)
                                  (search "MISSING:" line :test #'char-equal))
                                lines))
         (verdict-word (when verdict-line
                         (string-trim
                          '(#\Space #\Tab #\.)
                          (subseq verdict-line
                                  (+ (search "VERDICT:" verdict-line
                                             :test #'char-equal)
                                     8)))))
         (feedback (if missing-line
                       (string-trim
                        '(#\Space #\Tab)
                        (subseq missing-line
                                (+ (search "MISSING:" missing-line
                                           :test #'char-equal)
                                   8)))
                       "No specific feedback available")))
    (cond ((and verdict-word (search "INSUFFICIENT" verdict-word :test #'char-equal))
           (values nil feedback))
          ((and verdict-word (search "SUFFICIENT" verdict-word :test #'char-equal))
           (values t feedback))
          (t
           (%debug-log "WARNING parse-verdict-response: unparseable verdict ~S; ~
                        treating as SUFFICIENT~%" verdict-word)
           (values t feedback)))))
```

Note the order of the two `cond` clauses: the string "INSUFFICIENT" contains "SUFFICIENT" as a substring, so we must test for the longer word first. The agent function itself builds the prompt, calls the model, and delegates interpretation to the parser:

```lisp
(defun assess-sufficiency (user-query retrieved-chunks &key (model *rag-model*))
  "Evaluate whether RETRIEVED-CHUNKS provide sufficient context
    to answer USER-QUERY. Returns two values:
      1. SUFFICIENT-P — T if context is sufficient, NIL otherwise
      2. FEEDBACK — String describing what information is missing."
  (%debug-log "~%DEBUG assess-sufficiency: evaluating ~A chunks~%"
              (length retrieved-chunks))
  (let* ((context (format-retrieved-chunks retrieved-chunks))
         (prompt
           (format nil
                   "You are a Sufficient Context Agent in an agentic RAG system. ~
                    Your role is to evaluate whether the retrieved passages ~
                    contain enough information to fully answer the user's question.~%~
                    ~%User Question: ~A~%~
                    ~%Retrieved Passages:~A~%~
                    ~%Evaluate carefully:~
                    ~%1. Does the context contain ALL the specific facts needed?~
                    ~%2. Are there any parts of the question left unanswered?~
                    ~%3. Is any critical information missing?~
                    ~%~%Respond in EXACTLY this format:~
                    ~%VERDICT: SUFFICIENT or INSUFFICIENT~
                    ~%REASON: (one sentence explaining your assessment)~
                    ~%MISSING: (if insufficient, describe what specific ~
                    information to search for next; if sufficient, write NONE)"
                   user-query context))
         (response (rag-generate prompt :model model)))
    (%debug-log "DEBUG assess-sufficiency response:~%~A~%" response)
    (multiple-value-bind (sufficient-p feedback)
        (parse-verdict-response response)
      (%debug-log "DEBUG assess-sufficiency: verdict=~A~%"
                  (if sufficient-p "SUFFICIENT" "INSUFFICIENT"))
      (values sufficient-p feedback))))
```

The two return values, **sufficient-p** (a boolean) and **feedback** (a string describing what's missing), drive the orchestrator's decision to either synthesize an answer or refine the search.

### Agent 4: The Synthesis Agent

When the context is deemed sufficient, the Synthesis Agent generates the final answer. It is instructed to use only the retrieved passages and to cite source filenames:

```lisp
(defun synthesize-answer (user-query retrieved-chunks &key (model *rag-model*))
  "Generate a grounded answer to USER-QUERY using RETRIEVED-CHUNKS.
    The answer cites source documents."
  (%debug-log "~%DEBUG synthesize-answer: generating answer from ~A chunks~%"
              (length retrieved-chunks))
  (let* ((context (format-retrieved-chunks retrieved-chunks))
         (prompt
           (format nil
                   "You are a Synthesis Agent in a RAG system. Generate a ~
                    clear, accurate answer to the user's question using ONLY ~
                    the information in the retrieved passages below. ~
                    ~%~%Rules:~
                    ~%- Base your answer strictly on the retrieved passages~
                    ~%- Cite sources by mentioning the source filename~
                    ~%- If the passages don't fully answer the question, ~
                    say what you can answer and note what's missing~
                    ~%- Be concise but thorough~
                    ~%~%User Question: ~A~
                    ~%~%Retrieved Passages:~A"
                   user-query context))
         (response (rag-generate prompt :model model)))
    (%debug-log "DEBUG synthesize-answer: generated response (~A chars)~%"
                (length response))
    response))
```

### The Orchestrator

The **agentic-rag** function ties everything together. It runs the full pipeline, iterating when the Sufficient Context Agent determines the retrieved passages are incomplete:

```lisp
(defun agentic-rag (corpora user-query &key (max-iterations 3)
                                            (top-k 3)
                                            (model *rag-model*)
                                            (max-context-chunks 8))
  "Run the full agentic RAG pipeline:
      1. Rewrite the user query into sub-queries
      2. Search corpora for relevant chunks
      3. Check if context is sufficient (loop if not)
      4. Synthesize a grounded answer

    CORPORA is a list of corpus structs.
    MODEL is the Gemini model id used for every agent call.
    MAX-CONTEXT-CHUNKS caps how many retrieved passages are sent to the
    LLM (highest-scoring first) no matter how many iterations ran.
    Returns the synthesized answer string."
  (%debug-log "~%~%========================================~%")
  (%debug-log "  AGENTIC RAG PIPELINE~%")
  (%debug-log "  Query: ~A~%" user-query)
  (%debug-log "========================================~%")

  ;; Phase 1: Rewrite queries
  (let* ((sub-queries (rewrite-queries user-query :model model))
         ;; Phase 2: Initial search
         (all-chunks (search-fanout corpora sub-queries :top-k top-k))
         (iteration 0))

    ;; Phase 3: Iterative sufficiency check
    (loop
      (incf iteration)
      (%debug-log "~%--- Iteration ~A/~A ---~%" iteration max-iterations)

      (when (null all-chunks)
        (%debug-log "DEBUG agentic-rag: no chunks found, returning empty answer~%")
        (return-from agentic-rag
          "I could not find any relevant information in the available documents."))

      ;; Cap prompt size regardless of how many iterations accumulated chunks
      (let ((context-chunks (%cap-context all-chunks max-context-chunks)))

        ;; At the last allowed iteration both branches end in "synthesize
        ;; with what we have", so skip the sufficiency LLM call entirely.
        (when (>= iteration max-iterations)
          (%debug-log "~%DEBUG agentic-rag: max iterations reached, synthesizing ~
                       with available context~%")
          (return-from agentic-rag
            (synthesize-answer user-query context-chunks :model model)))

        (multiple-value-bind (sufficient-p feedback)
            (assess-sufficiency user-query context-chunks :model model)

          (when sufficient-p
            (%debug-log "~%DEBUG agentic-rag: context is SUFFICIENT at iteration ~A~%"
                        iteration)
            ;; Phase 5: Synthesize answer
            (return-from agentic-rag
              (synthesize-answer user-query context-chunks :model model)))

          ;; Phase 4: Refine and search again
          (%debug-log "~%DEBUG agentic-rag: context INSUFFICIENT, refining...~%")
          (%debug-log "DEBUG agentic-rag: feedback: ~A~%" feedback)
          (let* ((refined-queries (refine-queries user-query feedback
                                                  :model model))
                 (new-chunks (search-fanout corpora refined-queries
                                            :top-k top-k)))
            ;; Accumulate new chunks with existing ones (deduplicate by
            ;; (source . text) so identical text from different files
            ;; stays distinct)
            (let ((seen (make-hash-table :test 'equal)))
              (dolist (scored-chunk all-chunks)
                (setf (gethash (document-chunk-key (cdr scored-chunk)) seen) t))
              (dolist (scored-chunk new-chunks)
                (unless (gethash (document-chunk-key (cdr scored-chunk)) seen)
                  (setf (gethash (document-chunk-key (cdr scored-chunk)) seen) t)
                  (push scored-chunk all-chunks))))
            ;; Re-sort by score
            (setf all-chunks (sort all-chunks #'> :key #'car))))))))
```

Two details of the loop are worth pointing out. First, all progress output goes through **%debug-log**, so binding **\*rag-verbose\*** to NIL silences the entire pipeline, banner and iterations included; the orchestrator prints nothing on its own authority. Second, the `max-iterations` check runs *before* **assess-sufficiency**. At the final iteration, both a SUFFICIENT and an INSUFFICIENT verdict end in "synthesize with what we have", so the assessment cannot change the outcome; asking the model anyway spends a call to learn nothing. With `max-iterations 3` and a query that never converges, the pipeline makes two assessment calls, not three.

Notice how each iteration accumulates new chunks with the existing ones, deduplicating by (source . text). The accumulated context grows richer with each iteration, increasing the likelihood that the Sufficient Context Agent will be satisfied. The **max-context-chunks** keyword caps how many top-scoring passages are actually sent to the model, so a long refinement loop cannot grow the prompt without bound.

## Top-Level API and Demo

The file **rag.lisp** provides convenience functions and a built-in demo. The **test** function creates three separate corpora (renewable energy, electric vehicles, and climate science) and runs three progressively harder queries:

```lisp
(defun test ()
  "Run a demo of the Agentic RAG system with sample documents.
   Creates three corpora (energy, vehicles, climate) and runs
   multi-hop queries that require cross-corpus retrieval."
  (format t "~%~%============================================~%")
  (format t "  Agentic RAG Demo — Loading Documents~%")
  (format t "============================================~%")

  ;; Create three separate corpora to demonstrate cross-corpus retrieval
  (let ((energy-corpus (make-corpus :name "renewable-energy"
                                    :description "Renewable energy sources and technologies"))
        (ev-corpus (make-corpus :name "electric-vehicles"
                                :description "Electric vehicle technology and infrastructure"))
        (climate-corpus (make-corpus :name "climate-science"
                                    :description "Climate science and carbon emissions")))
    
    ;; Load documents into their respective corpora
    (add-document energy-corpus (data-path "renewable-energy.txt"))
    (add-document ev-corpus (data-path "electric-vehicles.txt"))
    (add-document climate-corpus (data-path "climate-science.txt"))
    
    (let ((all-corpora (list energy-corpus ev-corpus climate-corpus)))
      (format t "~%~%Loaded ~A total chunks across ~A corpora.~%"
              (loop for c in all-corpora sum (corpus-chunk-count c))
              (length all-corpora))
      
      ;; Query 1: Single-corpus question (should find answer easily)
      (format t "~%~%===== TEST QUERY 1 (single topic) =====~%")
      (let ((answer (query all-corpora
                           "What is the current cost of lithium-ion battery storage per kilowatt-hour?")))
        (format t "~%~%ANSWER 1:~%~A~%~%" answer))
      
      ;; Query 2: Multi-hop question requiring cross-corpus retrieval
      (format t "~%~%===== TEST QUERY 2 (multi-hop, cross-corpus) =====~%")
      (let ((answer (query all-corpora
                           "How does the carbon footprint of manufacturing EV batteries compare to the emissions saved by charging EVs from renewable energy sources?")))
        (format t "~%~%ANSWER 2:~%~A~%~%" answer))
      
      ;; Query 3: Complex question that may need iterative retrieval
      (format t "~%~%===== TEST QUERY 3 (complex, iterative) =====~%")
      (let ((answer (query all-corpora
                           "What role could solid-state batteries and pumped-storage hydroelectricity play together in solving the intermittency problem of wind and solar energy?")))
        (format t "~%~%ANSWER 3:~%~A~%~%" answer))
      
      (format t "~%~%============================================~%")
      (format t "  Demo Complete~%")
      (format t "============================================~%")
      
      ;; Return corpora for interactive use
      all-corpora)))
```

The test queries are designed to demonstrate different capabilities:

1. **Query 1** is a simple factual lookup: the answer exists in a single document chunk.
2. **Query 2** requires combining information from the electric vehicles corpus (battery manufacturing emissions) with the climate science corpus (emissions data), demonstrating cross-corpus retrieval. The documents do not actually contain the break-even data the question asks for, so this query also shows the refinement loop running to exhaustion and the synthesis agent reporting what it can and cannot answer.
3. **Query 3** combines solid-state batteries (EV corpus) with pumped-storage hydro and hybrid storage (renewable energy corpus). Here the initial retrieval already finds the hybrid-storage passage, and the Sufficient Context Agent accepts on the first iteration.

## Running the Example

Load the system and run the demo:

```
$ sbcl
* (load "project.lisp")

--- rag project loaded ---

* (rag:test)

============================================
  Agentic RAG Demo — Loading Documents
============================================

DEBUG add-document: loading .../data/renewable-energy.txt
DEBUG add-document: split into 9 chunks

DEBUG get-embeddings: batch-fetching 9 embeddings
DEBUG add-document: added 9 chunks from renewable-energy.txt

DEBUG add-document: loading .../data/electric-vehicles.txt
DEBUG add-document: split into 7 chunks

DEBUG get-embeddings: batch-fetching 7 embeddings
DEBUG add-document: added 7 chunks from electric-vehicles.txt

DEBUG add-document: loading .../data/climate-science.txt
DEBUG add-document: split into 7 chunks

DEBUG get-embeddings: batch-fetching 7 embeddings
DEBUG add-document: added 7 chunks from climate-science.txt

Loaded 23 total chunks across 3 corpora.


===== TEST QUERY 1 (single topic) =====

========================================
  AGENTIC RAG PIPELINE
  Query: What is the current cost of lithium-ion battery storage
         per kilowatt-hour?
========================================

DEBUG rewrite-queries: decomposing query...
DEBUG rewrite-queries: generated 3 sub-queries:
  - lithium-ion battery storage cost per kWh 2024
  - recent trends in lithium-ion battery pack prices per kilowatt-hour
  - average cost per kWh for utility-scale lithium-ion batteries

DEBUG search-fanout: searching 3 corpora with 4 queries

DEBUG get-embeddings: batch-fetching 4 embeddings
DEBUG search-fanout: searching with: "lithium-ion battery storage cost per kWh 2024"
DEBUG search-fanout: searching with: "recent trends in lithium-ion battery pack prices per kilowatt-hour"
DEBUG search-fanout: searching with: "average cost per kWh for utility-scale lithium-ion batteries"
DEBUG search-fanout: searching with: "What is the current cost of lithium-ion battery storage per kilowatt-hour?"
DEBUG search-fanout: found 5 unique chunks

--- Iteration 1/3 ---

DEBUG assess-sufficiency: evaluating 5 chunks
DEBUG assess-sufficiency response:
VERDICT: SUFFICIENT
REASON: The first retrieved passage provides a specific current cost
figure, stating that the price of lithium-ion battery storage has
fallen to under $140 per kilowatt-hour.
MISSING: NONE
DEBUG assess-sufficiency: verdict=SUFFICIENT

DEBUG agentic-rag: context is SUFFICIENT at iteration 1

DEBUG synthesize-answer: generating answer from 5 chunks


ANSWER 1:
Based on the provided passages, the current cost of lithium-ion
battery storage is under $140 per kilowatt-hour (source:
renewable-energy.txt). The cost has fallen by approximately 90% since
2010, when prices were over $1,100 per kilowatt-hour (source:
renewable-energy.txt).
```

Two things are worth noticing in this run. The rewriter produced three sub-queries, but the fanout searched with four: the original question is appended to the sub-query list, and all four embeddings are fetched with one batched call ("batch-fetching 4 embeddings"). Each document load is likewise one batched request.

Query 2 is the interesting case for the refinement loop. The question asks for a comparison the documents cannot fully support, so the Sufficient Context Agent keeps finding the gap and the loop runs to the iteration limit:

```
===== TEST QUERY 2 (multi-hop, cross-corpus) =====

--- Iteration 1/3 ---

DEBUG assess-sufficiency response:
VERDICT: INSUFFICIENT
REASON: While the passages provide the carbon footprint for battery
manufacturing (75-100 kg CO2/kWh), they lack specific quantitative
data on the emissions saved per mile or year to allow for a direct
comparison or break-even analysis.
MISSING: Quantitative data on CO2 emissions from internal combustion
engine vehicles or the "break-even" distance/time required for an EV
charged by renewables to offset its manufacturing carbon footprint.

DEBUG agentic-rag: context INSUFFICIENT, refining...
DEBUG refine-queries: generated 2 refined queries:
  - lifecycle CO2 emissions per mile internal combustion engine vs EV
    battery manufacturing data
  - break-even driving distance EV charged by renewables to offset
    battery production emissions

DEBUG search-fanout: found 4 unique chunks

--- Iteration 2/3 ---

DEBUG assess-sufficiency: evaluating 6 chunks
DEBUG assess-sufficiency response:
VERDICT: INSUFFICIENT
REASON: While the passages provide the carbon footprint for battery
manufacturing (75-100 kg CO2/kWh), they lack the specific emission
data for gasoline vehicles needed to calculate the "emissions saved"
or a direct break-even analysis.
MISSING: Average carbon emissions of internal combustion engine
vehicles per mile and the specific distance or timeframe required for
an EV charged on renewables to offset its manufacturing carbon debt.

--- Iteration 3/3 ---

DEBUG agentic-rag: max iterations reached, synthesizing with available context

DEBUG synthesize-answer: generating answer from 7 chunks


ANSWER 2:
Manufacturing EV batteries produces approximately 75-100 kg of CO2
per kilowatt-hour of battery capacity (source: electric-vehicles.txt).
When an EV is charged using renewable energy sources like solar or
wind, it produces zero operational emissions (source:
electric-vehicles.txt). Even when accounting for manufacturing, EVs
produce roughly 50-60% fewer lifecycle greenhouse gas emissions than
comparable gasoline vehicles when charged from the average US grid
mix (source: electric-vehicles.txt).

Missing Information: The retrieved passages do not provide a specific
"break-even" point (such as the number of miles or years) at which
the emissions saved by renewable charging fully offset the initial
CO2 generated during battery manufacturing.
```

Notice that iteration 3/3 goes straight to synthesis: the sufficiency check is skipped entirely at the last iteration because its verdict cannot change the outcome. The pipeline honestly reports what it found and what is missing, which is the right behavior when the documents simply do not contain the requested data.

After the test completes, you can use the returned corpora for interactive queries:

```
* (defvar *corpora* (rag:test))
;; ... test output ...

* (rag:interactive-demo *corpora*)

============================
  Agentic RAG Interactive Demo
============================

Loaded 3 corpora with 23 total chunks.
Type your question (or 'quit' to exit):

RAG> What is the Paris Agreement temperature target?

===== ANSWER =====
The Paris Agreement aims to limit warming to 1.5°C above
pre-industrial levels (source: climate-science.txt).
==================

RAG> quit
```

When **rag:test** returns the corpora and the REPL prints the return value, the custom print functions from the vector store section keep the output readable: each corpus is one line with its name, description, and chunk count, and inspecting a single chunk shows only the first 10 embedding values plus the dimension count. Without those printers, this one expression would dump all 23 chunks with their full 3072-value embeddings.

## Offline Tests

Because **\*embedding-fn\***, **\*batch-request-fn\***, and **\*generate-fn\*** are special variables holding functions, the whole pipeline can be exercised without network access or an API key:

```
* (asdf:test-system :rag)

All RAG tests passed.
```

The test system (**tests.lisp**, package `rag-tests`) covers the chunking edge cases (including the forward-progress guard discussed above), query line parsing (queries that begin with digits or decimal numbers survive; list prefixes of any length are stripped), vector math (including the dimension-mismatch error), retrieval ranking and deduplication (identical text from different files stays distinct), batched query embedding (one call for the whole fanout), batch splitting at the 100-text API cap, cache eviction at the cap, retry behavior (transient 429/5xx and connection errors retry; permanent 4xx signal immediately), verdict parsing (including the INSUFFICIENT-contains-SUFFICIENT substring trap), corpus save/load round-trips with corrupt-file rejection, and a full `agentic-rag` run against a stubbed LLM, including the checks that the last iteration skips the sufficiency call and that a quiet pipeline prints nothing. There is no testing framework dependency; a small `check` macro records failures and `run-tests` signals an error if any occurred, which is all ASDF needs to report test failure.

## Wrap Up for Agentic RAG

The key takeaway from this chapter is that agentic RAG dramatically improves answer quality compared to vanilla RAG, especially for complex queries that require information from multiple sources. The Sufficient Context Agent is the critical innovation: by explicitly checking whether enough information has been retrieved before generating an answer, we avoid the common failure modes of hallucination and incomplete responses.

The implementation is deliberately simple: each "agent" is just a function with a well-crafted prompt. You don't need an elaborate agent framework to get the benefits of multi-agent architectures. What matters is the pattern: decompose, search, assess, refine, synthesize.

The engineering around that pattern is deliberately practical as well: embeddings are stored as normalized simple-vectors computed with batched litelm calls and memoized, all sub-queries in a fanout share one embedding request, transport, JSON, and API-key handling are delegated to litelm, transient HTTP and connection failures are retried while permanent client errors surface immediately, corpora can be saved to disk, validated on load, and reloaded without re-embedding, and every network-facing function sits behind a rebindable special variable so the entire pipeline is testable offline.

For production use, consider these enhancements:

- **Persistent vector store**: Replace the in-memory lists with a dedicated vector database (Chroma, Qdrant, or Pinecone) for larger document collections. Because chunks are stored normalized, a store that indexes unit vectors can use plain dot-product scoring directly.
- **Document loaders**: Add support for PDF, HTML, and other formats beyond plain text.
- **Structured agent outputs**: Request schema-constrained responses from the chat model instead of parsing VERDICT lines (see practice problem 5).
- **Parallel search**: Use threads to search multiple corpora simultaneously (see practice problem 6).

The Google research reports that their production agentic RAG system achieves up to 34% higher accuracy than vanilla RAG on factuality benchmarks, with cross-corpus retrieval nearly matching single-corpus accuracy. Our Common Lisp implementation demonstrates the same architecture on a smaller scale.

## Optional Practice Problems

1. **Custom Chunk Size and Overlap Strategy**:
   The logic inside `split-into-chunks` in [vector-store.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/vector-store.lisp) uses the package constants `*default-chunk-size*` (500 characters) and `*chunk-overlap*` (50 characters). Modify `add-document` in [vector-store.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/vector-store.lisp) and `agentic-rag` in [agents.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/agents.lisp) to support dynamic configuration of these parameters. Write a helper function that measures the sensitivity of retrieval relevance scores to different chunk configurations.

2. **Deduplication with Embedding Similarity (Soft Deduplication)**:
   In `search-fanout` (in [agents.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/agents.lisp)), the retrieved chunks are deduplicated by exact `document-chunk-key` matching (source plus text). In large datasets, different documents might contain near-identical chunks or rephrased content. Implement a "soft" deduplication mechanism in `search-fanout` that uses the `cosine-similarity` function from [embeddings.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/embeddings.lisp) to discard any retrieved chunk that has a similarity score greater than 0.9 with an already selected chunk. Remember that stored embeddings are normalized, so the comparison is a plain dot product.

3. **Multi-Turn Chat Interface Integration**:
   The current `interactive-demo` loop in [rag.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/rag.lisp) and the orchestrator `agentic-rag` in [agents.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/agents.lisp) are stateless: each query is processed independently. Extend the pipeline to support a conversation history (list of past QA turns). Pass the history to the `Query Rewriter` so it can resolve pronouns and context (e.g., rewriting "How does it compare to hydro?" following "What is the cost of battery storage?").

4. **Self-Correction and Web Search Fallback on Refinement**:
   In `agentic-rag` (in [agents.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/agents.lisp)), if the context remains insufficient after refining queries, the system continues to search the same corpus. Write a fallback mechanism that, when the Sufficient Context Agent reports `INSUFFICIENT` for the second time, switches to an external API (like a local DuckDuckGo lookup or Ollama Cloud web search helper) to gather external context, appending the results to the RAG vector store dynamically.

5. **Structured Agent Verdicts**:
   The Sufficient Context Agent in [agents.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/agents.lisp) relies on string matching (searching for `"VERDICT:"` and `"MISSING:"`) to parse the model's reply. This is fragile if the model emits code blocks, explanations, or formatting deviations. Modify `assess-sufficiency` to get a structured verdict instead: either ask for JSON and decode it with `litelm:json-decode`, or define a `record_verdict` tool with `verdict`, `reason`, and `missing` parameters, pass it to `litelm:completion` through `:tools`, and read the arguments out of `litelm:response-tool-calls`. Compare how often each approach parses correctly across a set of deliberately awkward prompts.

6. **Parallelized Search Fanout**:
   The `search-fanout` function in [agents.lisp](file:///Users/markw/GITHUB/loving-common-lisp/src/RAG/agents.lisp) embeds all sub-queries in one batched API call, but still searches them sequentially: each sub-query's `search-corpora` call runs one after another. When there are several sub-queries and multiple corpora, searching one by one adds latency. Use a threading library such as `bordeaux-threads` to parallelize the `search-corpora` calls per sub-query, gathering and deduplicating results once all threads terminate.
