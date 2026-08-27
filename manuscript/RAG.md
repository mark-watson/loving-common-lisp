# Agentic RAG Using the Gemini LLM APIs

This chapter implements an **Agentic Retrieval-Augmented Generation (RAG)** system in Common Lisp, inspired by Google's June 2026 research blog post [Unlocking Dependable Responses with Agentic RAG](https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/).

In the previous chapter on document question answering, we built a "vanilla" RAG system: embed documents, embed the query, find similar chunks, and pass them to an LLM for answer generation. That approach works well for simple factual questions, but falls short on complex queries that require information from multiple sources or where the first retrieval pass misses critical details.

Agentic RAG addresses this limitation by introducing multiple specialized agents that **plan, rewrite queries, assess context sufficiency, and iteratively search** until enough information is gathered to produce a reliable answer. The key insight from the Google research is the **Sufficient Context Agent** — a quality-control step that evaluates whether the retrieved passages actually contain enough information to answer the question, and if not, generates specific feedback about what's missing so the system can refine its search.

The source code for this example is in the directory **src/RAG** of the book's GitHub repository. It uses the Gemini `gemini-embedding-001` model for embeddings (free tier) and `gemini-3-flash-preview` for all agent LLM calls (very inexpensive).

## Overview of the Agentic RAG Architecture

The system implements a multi-agent pipeline with five phases:

1. **Query Rewriting** — A Gemini-powered agent decomposes complex questions into 1–3 focused sub-queries for retrieval.
2. **Search Fanout** — Each sub-query is embedded and searched across multiple document corpora. Results are deduplicated.
3. **Sufficient Context Assessment** — A specialized agent evaluates whether the retrieved passages contain enough information to fully answer the original question.
4. **Iterative Refinement** — If context is insufficient, the system generates refined search queries based on feedback about what's missing, then searches again. This loop repeats up to a configurable limit.
5. **Synthesis** — Once context is sufficient (or the iteration limit is reached), a synthesis agent generates a grounded answer citing source documents.

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
  :description "Agentic RAG (Retrieval-Augmented Generation) using Gemini"
  :author "Mark Watson"
  :license "Apache 2"
  :version "1.0.0"
  :serial t
  :depends-on (#:llm #:cl-json #:dexador #:uiop)
  :components ((:file "package")
               (:file "embeddings")
               (:file "vector-store")
               (:file "agents")
               (:file "rag"))
  :in-order-to ((asdf:test-op (asdf:test-op #:rag/test))))

(asdf:defsystem #:rag/test
  :description "Offline unit tests for the rag system (no network access)."
  :depends-on (#:rag #:uiop)
  :serial t
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :rag-tests :run-tests)))
```

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
           #:*rag-model*))
```

## Computing Embeddings With the Gemini API

The file **embeddings.lisp** provides the foundation for semantic search. We use Google's `gemini-embedding-001` model, which produces 3072-dimensional vectors and is available on the free tier.

All HTTP access goes through **llm:post-json** from the **llm** library we built earlier, so there is a single HTTP path shared with the rest of the book's examples. The Dexador HTTP library signals a `dex:http-request-failed` condition when the API returns an error status, which lets us implement automatic retry with exponential backoff. This matters in practice because the free tier rate limits are easy to hit while embedding a batch of documents:

```lisp
(defun call-with-retries (thunk &key (attempts 3) (initial-delay 1.0))
  "Call THUNK, retrying on transient HTTP failures with exponential
   backoff (1s, 2s, 4s by default)."
  (loop for attempt from 1
        for delay = initial-delay then (* delay 2)
        do (handler-case (return (funcall thunk))
             (dex:http-request-failed (e)
               (when (>= attempt attempts)
                 (error "Gemini API request failed after ~A attempts: ~A"
                        attempts e))
               (%debug-log "~%DEBUG call-with-retries: attempt ~A/~A failed, ~
                            retrying in ~A seconds~%" attempt attempts delay)
               (sleep delay)))))
```

Debug output throughout the system goes through the **%debug-log** macro, which prints only when **\*rag-verbose\*** is true. The verbose tracing is valuable when following the pipeline in this chapter, and setting the variable to NIL turns the system into a quiet library.

Embeddings are memoized in **\*embedding-cache\***, a hash table keyed on the model name and text, so reloading a document or re-running the demo never pays for the same API call twice. The low-level function **%fetch-embedding** calls the `embedContent` endpoint for a single string:

```lisp
(defvar *embedding-model* "gemini-embedding-001")

(defvar *embedding-fn* #'%fetch-embedding
  "Function of one argument (a string) returning an embedding vector.
   Rebind this in tests to run the pipeline without network access.")

(defun %fetch-embedding (text)
  "Compute an embedding vector for TEXT via the embedContent endpoint."
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
```

Note the special variable **\*embedding-fn\***: the public entry point **get-embedding** calls whatever function it holds. Defaulting it to the real HTTP implementation while allowing tests to rebind it to a stub is a simple Common Lisp idiom we will use again for the LLM calls, and it is what makes the offline unit tests possible.

Two public functions round out the interface. **get-embedding** checks the cache before calling the API, and **get-embeddings** (not shown) fetches all cache misses for a list of texts with a single `batchEmbedContents` API call, turning N HTTP requests into one when a document is first loaded:

```lisp
(defun get-embedding (text)
  "Compute (or retrieve from cache) an embedding vector for TEXT."
  (let ((key (embedding-cache-key text)))
    (multiple-value-bind (cached present-p) (gethash key *embedding-cache*)
      (if present-p
          cached
          (let ((vec (funcall *embedding-fn* text)))
            (setf (gethash key *embedding-cache*) vec)
            vec)))))
```

We also define **cosine-similarity** to compare two embedding vectors; this is how we determine which document chunks are most relevant to a query:

```lisp
(defun dot-product (vec-a vec-b)
  (loop for a in vec-a for b in vec-b sum (* a b)))

(defun vector-magnitude (vec)
  (sqrt (loop for x in vec sum (* x x))))

(defun cosine-similarity (vec-a vec-b)
  "Compute cosine similarity between two embedding vectors.
   Returns a value between -1 and 1."
  (let ((mag-a (vector-magnitude vec-a))
        (mag-b (vector-magnitude vec-b)))
    (if (or (zerop mag-a) (zerop mag-b))
        0.0
        (/ (dot-product vec-a vec-b) (* mag-a mag-b)))))
```

The embedding API returns a JSON response containing a list of floating-point values. The cosine similarity between two vectors measures how similar their directions are in the high-dimensional embedding space, regardless of magnitude. A similarity of 1.0 means the texts are semantically identical; 0.0 means they are unrelated.

## In-Memory Vector Store

The file **vector-store.lisp** implements a simple in-memory document store. Production systems would use a dedicated vector database like Pinecone or Chroma, but for a book example, an in-memory list with brute-force cosine similarity is clearer and requires zero setup.

We define two structs: **document-chunk** holds a piece of text with its source filename and embedding vector, and **corpus** is a named collection of chunks:

```lisp
(defstruct document-chunk
  "A chunk of text with its source file and embedding vector."
  text
  source
  embedding)

(defstruct corpus
  "A named collection of document chunks for retrieval."
  name
  description
  (chunks nil))
```

The function **split-into-chunks** breaks a long text into overlapping pieces of approximately 500 characters each, trying to break at sentence boundaries (periods or newlines) rather than cutting words in half:

```lisp
(defvar *default-chunk-size* 500)
(defvar *chunk-overlap* 50)

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

The function **add-document** reads a file, chunks it, and computes embeddings for all chunks at once. When the default embedding function is in use, **get-embeddings** makes a single batch API call for the whole document instead of one request per chunk:

```lisp
(defun add-document (corpus filepath &key (chunk-size *default-chunk-size*))
  "Read a text file, split it into chunks, compute embeddings (in a
   single batch API call), and add the chunks to CORPUS."
  (let* ((text (uiop:read-file-string filepath))
         (chunks (split-into-chunks text :chunk-size chunk-size))
         (source (file-namestring filepath)))
    (setf (corpus-chunks corpus)
          (nconc (corpus-chunks corpus)
                 (loop for chunk-text in chunks
                       for embedding in (get-embeddings chunks)
                       collect (make-document-chunk :text chunk-text
                                                    :source source
                                                    :embedding embedding))))
    (length chunks)))
```

Because embedding a corpus costs API calls, **save-corpus** and **load-corpus** persist a corpus, embeddings included, as a plain s-expression file. Note the `*read-eval*` binding when loading: `read` on an untrusted file must never be allowed to evaluate embedded forms:

```lisp
(defun save-corpus (corpus pathname)
  "Write CORPUS (name, description, chunks with embeddings) to PATHNAME
   as a single s-expression. Load it back with LOAD-CORPUS."
  (with-open-file (out pathname :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (with-standard-io-syntax
      (prin1 (list :name (corpus-name corpus)
                   :description (corpus-description corpus)
                   :chunks (mapcar (lambda (chunk)
                                     (list :text (document-chunk-text chunk)
                                           :source (document-chunk-source chunk)
                                           :embedding (document-chunk-embedding chunk)))
                                   (corpus-chunks corpus)))
             out)))
  pathname)

(defun load-corpus (pathname)
  "Load a corpus previously written by SAVE-CORPUS."
  (with-open-file (in pathname :direction :input)
    (with-standard-io-syntax
      (let* ((*read-eval* nil) ; never evaluate while reading data files
             (data (read in))
             (corpus (make-corpus :name (getf data :name)
                                  :description (getf data :description))))
        (setf (corpus-chunks corpus)
              (mapcar (lambda (chunk-data)
                        (make-document-chunk :text (getf chunk-data :text)
                                             :source (getf chunk-data :source)
                                             :embedding (getf chunk-data :embedding)))
                      (getf data :chunks)))
        corpus))))
```

The **search-corpus** and **search-corpora** functions find the top-K most similar chunks for a given query embedding:

```lisp
(defun search-corpora (corpora query-embedding &key (top-k 3))
  "Search multiple CORPORA for the TOP-K most similar chunks overall.
   Returns a list of (score . document-chunk) pairs."
  (let ((all-results
          (loop for corpus in corpora
                append (search-corpus corpus query-embedding
                                      :top-k top-k))))
    (subseq (sort all-results #'> :key #'car)
            0 (min top-k (length all-results)))))
```

An important feature for agentic RAG is that **search-corpora** accepts a list of corpora, enabling cross-corpus retrieval. The Google research article emphasizes this capability: real-world knowledge is often spread across separate databases managed by different teams. Our system searches all corpora simultaneously and returns the best results regardless of source.

## The Multi-Agent Pipeline

The file **agents.lisp** is the heart of the system. Each "agent" is a function that calls Gemini with a specialized prompt. This is a practical and effective pattern — we don't need an external agent framework to implement agent behaviors, just well-crafted prompts and structured response parsing.

We use `gemini-3-flash-preview` for all agent calls. This model is very inexpensive while being capable enough for query rewriting, sufficiency assessment, and synthesis. The function **rag-generate** delegates to **gemini:generate** from the **llm** library, going through the special variable **\*generate-fn\*** so tests can substitute a stub (the same idiom as **\*embedding-fn\*** above):

```lisp
(defvar *rag-model* "gemini-3-flash-preview"
  "Gemini model used for all agent LLM calls. Override per call with
   the :model keyword argument to agentic-rag.")

(defvar *generate-fn*
  (lambda (prompt &key (model *rag-model*))
    (gemini:generate prompt :model-id model))
  "Function of (prompt &key model) returning generated text. Defaults
   to a thin wrapper around gemini:generate from the llm library.
   Rebind this in tests to run the pipeline without network access.")

(defun rag-generate (prompt &key (model *rag-model*))
  "Call the LLM through *generate-fn* and return the generated text.
   Signals an error if the model returns no text."
  (or (funcall *generate-fn* prompt :model model)
      (error "LLM returned no text for prompt: ~A"
             (subseq prompt 0 (min 80 (length prompt))))))
```

### Agent 1: The Query Rewriter

The Query Rewriter takes a complex user question and decomposes it into 1–3 focused sub-queries. For example, the question "How does the carbon footprint of manufacturing EV batteries compare to the emissions saved by charging EVs from renewable energy?" would be split into sub-queries like:

- "carbon footprint of EV battery manufacturing"
- "emissions saved by charging electric vehicles from renewable energy"

This decomposition improves retrieval because each sub-query targets a specific fact that might appear in a different document or section.

```lisp
(defun parse-query-lines (response)
  "Extract one query per line from a rewriter agent RESPONSE."
  (remove-if (lambda (s) (zerop (length s)))
             (mapcar (lambda (line)
                       (string-trim '(#\Space #\Tab #\- #\* #\1 #\2 #\3 #\.)
                                    line))
                     (uiop:split-string (or response "")
                                        :separator '(#\Newline)))))

(defun rewrite-queries (user-query &key (model *rag-model*))
  "Decompose USER-QUERY into 1-3 focused sub-queries for retrieval.
   Returns a list of query strings."
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
    (if queries
        queries
        (list user-query))))
```

### Agent 2: Search Fanout

The Search Fanout agent executes the sub-queries against all corpora. For each sub-query, it computes an embedding and searches for the most similar document chunks. Results are deduplicated to avoid showing the same passage twice when multiple sub-queries match the same text.

```lisp
(defun search-fanout (corpora sub-queries &key (top-k 3))
  "Execute embedding search across CORPORA for each sub-query.
   Returns a deduplicated list of (score . document-chunk) pairs,
   sorted by descending score."
  (%debug-log "~%DEBUG search-fanout: searching ~A corpora with ~A queries~%"
              (length corpora) (length sub-queries))
  (let ((all-results nil)
        (seen-texts (make-hash-table :test 'equal)))
    (dolist (query sub-queries)
      (%debug-log "DEBUG search-fanout: embedding query: ~S~%" query)
      (let* ((query-embedding (get-embedding query))
             (results (search-corpora corpora query-embedding :top-k top-k)))
        (dolist (result results)
          (let ((text (document-chunk-text (cdr result))))
            (unless (gethash text seen-texts)
              (setf (gethash text seen-texts) t)
              (push result all-results))))))
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
           (%debug-log "WARNING parse-verdict-response: unparseable verdict ~S~%"
                       verdict-word)
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

The two return values — **sufficient-p** (a boolean) and **feedback** (a string describing what's missing) — drive the orchestrator's decision to either synthesize an answer or refine the search.

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
  (format t "~%~%========================================~%")
  (format t "  AGENTIC RAG PIPELINE~%")
  (format t "  Query: ~A~%" user-query)
  (format t "========================================~%")

  ;; Phase 1: Rewrite queries
  (let* ((sub-queries (rewrite-queries user-query :model model))
         ;; Phase 2: Initial search
         (all-chunks (search-fanout corpora sub-queries :top-k top-k))
         (iteration 0))

    ;; Phase 3: Iterative sufficiency check
    (loop
      (incf iteration)
      (format t "~%--- Iteration ~A/~A ---~%" iteration max-iterations)

      (when (null all-chunks)
        (return-from agentic-rag
          "I could not find any relevant information in the available documents."))

      ;; Cap prompt size regardless of how many iterations accumulated chunks
      (let ((context-chunks (%cap-context all-chunks max-context-chunks)))
        (multiple-value-bind (sufficient-p feedback)
            (assess-sufficiency user-query context-chunks :model model)

          (when sufficient-p
            (%debug-log "~%DEBUG agentic-rag: context is SUFFICIENT at iteration ~A~%"
                        iteration)
            (return-from agentic-rag
              (synthesize-answer user-query context-chunks :model model)))

          (when (>= iteration max-iterations)
            (%debug-log "~%DEBUG agentic-rag: max iterations reached~%")
            (return-from agentic-rag
              (synthesize-answer user-query context-chunks :model model)))

          ;; Phase 4: Refine and search again
          (%debug-log "~%DEBUG agentic-rag: context INSUFFICIENT, refining...~%")
          (%debug-log "DEBUG agentic-rag: feedback: ~A~%" feedback)
          (let* ((refined-queries (refine-queries user-query feedback :model model))
                 (new-chunks (search-fanout corpora refined-queries :top-k top-k)))
            ;; Accumulate new chunks (deduplicate)
            (let ((seen (make-hash-table :test 'equal)))
              (dolist (scored-chunk all-chunks)
                (setf (gethash (document-chunk-text (cdr scored-chunk)) seen) t))
              (dolist (scored-chunk new-chunks)
                (unless (gethash (document-chunk-text (cdr scored-chunk)) seen)
                  (setf (gethash (document-chunk-text (cdr scored-chunk)) seen) t)
                  (push scored-chunk all-chunks))))
            (setf all-chunks (sort all-chunks #'> :key #'car))))))))
```

Notice how each iteration accumulates new chunks with the existing ones, deduplicating by text content. The accumulated context grows richer with each iteration, increasing the likelihood that the Sufficient Context Agent will be satisfied. The **max-context-chunks** keyword caps how many top-scoring passages are actually sent to the model, so a long refinement loop cannot grow the prompt without bound.

## Top-Level API and Demo

The file **rag.lisp** provides convenience functions and a built-in demo. The **test** function creates three separate corpora — renewable energy, electric vehicles, and climate science — and runs three progressively harder queries:

```lisp
(defun test ()
  "Run a demo of the Agentic RAG system with sample documents."
  (format t "~%~%============================================~%")
  (format t "  Agentic RAG Demo — Loading Documents~%")
  (format t "============================================~%")

  (let ((energy-corpus (make-corpus :name "renewable-energy"
                                    :description "Renewable energy sources and technologies"))
        (ev-corpus (make-corpus :name "electric-vehicles"
                                :description "Electric vehicle technology and infrastructure"))
        (climate-corpus (make-corpus :name "climate-science"
                                    :description "Climate science and carbon emissions")))
    
    (add-document energy-corpus (data-path "renewable-energy.txt"))
    (add-document ev-corpus (data-path "electric-vehicles.txt"))
    (add-document climate-corpus (data-path "climate-science.txt"))
    
    (let ((all-corpora (list energy-corpus ev-corpus climate-corpus)))

      ;; Query 1: Single-corpus question
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
      
      all-corpora)))
```

The test queries are designed to demonstrate different capabilities:

1. **Query 1** is a simple factual lookup — the answer exists in a single document chunk.
2. **Query 2** requires combining information from the electric vehicles corpus (battery manufacturing emissions) with the climate science corpus (emissions data), demonstrating cross-corpus retrieval.
3. **Query 3** combines solid-state batteries (EV corpus) with pumped-storage hydro and hybrid storage (renewable energy corpus). The first retrieval pass finds the technologies described separately, so the refinement loop runs and succeeds on the second iteration; we will watch this happen in the sample run below.

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
DEBUG add-document: loading .../data/electric-vehicles.txt
DEBUG add-document: split into 7 chunks
DEBUG get-embeddings: batch-fetching 7 embeddings
DEBUG add-document: loading .../data/climate-science.txt
DEBUG add-document: split into 7 chunks
DEBUG get-embeddings: batch-fetching 7 embeddings

Loaded 23 total chunks across 3 corpora.


===== TEST QUERY 1 (single topic) =====

========================================
  AGENTIC RAG PIPELINE
  Query: What is the current cost of lithium-ion battery storage
         per kilowatt-hour?
========================================

DEBUG rewrite-queries: generated 1 sub-queries:
  - lithium-ion battery storage cost per kilowatt-hour

--- Iteration 1/3 ---

DEBUG assess-sufficiency: verdict=SUFFICIENT

ANSWER 1:
The cost of lithium-ion battery storage has fallen by approximately
90% since 2010, from over $1,100 per kilowatt-hour to under $140
per kilowatt-hour (source: renewable-energy.txt).
```

Notice that loading each document costs a single batched embedding request. Query 3, which asks how solid-state batteries and pumped-storage hydroelectricity could work *together*, is the interesting case: the initial retrieval finds passages about each technology separately, the Sufficient Context Agent reports the combined information is missing, and the refinement pass then locates the hybrid-storage passage in **renewable-energy.txt**:

```
===== TEST QUERY 3 (complex, iterative) =====

--- Iteration 1/3 ---

DEBUG assess-sufficiency response:
VERDICT: INSUFFICIENT
REASON: The passages describe the individual characteristics of
solid-state batteries and pumped-storage hydroelectricity but not
how they would function together.
MISSING: Information on the complementary roles of solid-state
batteries and pumped-storage hydroelectricity in short-term versus
long-term grid energy storage.

DEBUG agentic-rag: context INSUFFICIENT, refining...
DEBUG refine-queries: generated 2 refined queries:
  - combining battery and pumped hydro storage for grid stability
  - hybrid energy storage systems short and long duration

--- Iteration 2/3 ---

DEBUG assess-sufficiency: verdict=SUFFICIENT

ANSWER 3:
Together, solid-state batteries and pumped-storage hydroelectricity
form a hybrid storage system that addresses wind and solar
intermittency on complementary time scales (source:
renewable-energy.txt). Solid-state batteries excel at
short-duration storage: they respond within milliseconds to smooth
output when clouds pass over a solar farm or wind speeds drop,
provide frequency regulation, and shift solar generation into the
evening demand peak. Pumped-storage hydroelectricity plays the
opposite role, providing bulk, long-duration storage that moves
large amounts of energy across many hours or days, using excess
electricity to pump water uphill and releasing it through turbines
during peak demand. In such a pairing, the batteries absorb the
fast, frequent charge cycles that would wear out mechanical
equipment, while pumped hydro handles the deep, infrequent
discharges that would otherwise require impractically large battery
banks, allowing wind and solar installations to deliver firm,
dispatchable power around the clock.
```

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

## Offline Tests

Because **\*embedding-fn\*** and **\*generate-fn\*** are special variables holding functions, the whole pipeline can be exercised without network access or an API key:

```
* (asdf:test-system :rag)

All RAG tests passed.
```

The test system (**tests.lisp**, package `rag-tests`) covers the chunking edge cases (including the forward-progress guard discussed above), vector math, retrieval ranking and deduplication, verdict parsing (including the INSUFFICIENT-contains-SUFFICIENT substring trap), corpus save/load round-trips, and a full `agentic-rag` run against a stubbed LLM that answers SUFFICIENT on the second iteration. There is no testing framework dependency; a small `check` macro records failures and `run-tests` signals an error if any occurred, which is all ASDF needs to report test failure.

## Wrap Up for Agentic RAG

The key takeaway from this chapter is that agentic RAG dramatically improves answer quality compared to vanilla RAG, especially for complex queries that require information from multiple sources. The Sufficient Context Agent is the critical innovation — by explicitly checking whether enough information has been retrieved before generating an answer, we avoid the common failure modes of hallucination and incomplete responses.

The implementation is deliberately simple: each "agent" is just a function with a well-crafted prompt. You don't need an elaborate agent framework to get the benefits of multi-agent architectures. What matters is the pattern: decompose, search, assess, refine, synthesize.

The engineering around that pattern is deliberately practical as well: embeddings are computed with one batched API call per document and memoized, transient HTTP failures are retried with exponential backoff, corpora can be saved to disk and reloaded without re-embedding, and every network-facing function sits behind a rebindable special variable so the entire pipeline is testable offline.

For production use, consider these enhancements:

- **Persistent vector store**: Replace the in-memory lists with a dedicated vector database (Chroma, Qdrant, or Pinecone) for larger document collections.
- **Document loaders**: Add support for PDF, HTML, and other formats beyond plain text.
- **Structured agent outputs**: Request JSON Schema-constrained responses from the Interactions API instead of parsing VERDICT lines (see practice problem 5).
- **Parallel search**: Use threads to search multiple corpora simultaneously.

The Google research reports that their production agentic RAG system achieves up to 34% higher accuracy than vanilla RAG on factuality benchmarks, with cross-corpus retrieval nearly matching single-corpus accuracy. Our Common Lisp implementation demonstrates the same architecture on a smaller scale.

## Optional Practice Problems

1. **Custom Chunk Size and Overlap Strategy**:
   The logic inside `split-into-chunks` in [vector-store.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/vector-store.lisp) uses the package constants `*default-chunk-size*` (500 characters) and `*chunk-overlap*` (50 characters). Modify `add-document` in [vector-store.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/vector-store.lisp) and `agentic-rag` in [agents.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/agents.lisp) to support dynamic configuration of these parameters. Write a helper function that measures the sensitivity of retrieval relevance scores to different chunk configurations.

2. **Deduplication with Embedding Similarity (Soft Deduplication)**:
   In `search-fanout` (in [agents.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/agents.lisp)), the retrieved chunks are deduplicated strictly by exact string matching. In large datasets, different documents might contain near-identical chunks or rephrased content. Implement a "soft" deduplication mechanism in `search-fanout` that uses the `cosine-similarity` function from [embeddings.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/embeddings.lisp) to discard any retrieved chunk that has a similarity score greater than 0.9 with an already selected chunk.

3. **Multi-Turn Chat Interface Integration**:
   The current `interactive-demo` loop in [rag.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/rag.lisp) and the orchestrator `agentic-rag` in [agents.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/agents.lisp) are stateless: each query is processed independently. Extend the pipeline to support a conversation history (list of past QA turns). Pass the history to the `Query Rewriter` so it can resolve pronouns and context (e.g., rewriting "How does it compare to hydro?" following "What is the cost of battery storage?").

4. **Self-Correction and Web Search Fallback on Refinement**:
   In `agentic-rag` (in [agents.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/agents.lisp)), if the context remains insufficient after refining queries, the system continues to search the same corpus. Write a fallback mechanism that, when the Sufficient Context Agent reports `INSUFFICIENT` for the second time, switches to an external API (like a local DuckDuckGo lookup or Ollama Cloud web search helper) to gather external context, appending the results to the RAG vector store dynamically.

5. **JSON Schema Schema Enforcement for Agent Verdicts**:
   The Sufficient Context Agent in [agents.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/agents.lisp) relies on string matching (searching for `"VERDICT:"` and `"MISSING:"`) to parse Gemini's response. This is fragile if the model outputs code blocks, explanations, or formatting deviations. Modify `assess-sufficiency` to request structured output using a JSON Schema (by specifying schema parameters in the request payload to the Gemini Interactions API). Parse the returned structured JSON reliably using `cl-json`.

6. **Parallelized Search Fanout**:
   The `search-fanout` function in [agents.lisp](file:///Users/markwatson/GITHUB/loving-common-lisp/src/RAG/agents.lisp) runs queries sequentially. If there are 3 sub-queries and multiple corpora, fetching embeddings and searching them one by one introduces latency. Use a threading library such as `bordeaux-threads` to parallelize the calls to `get-embedding` and `search-corpora` for each sub-query, gathering and deduplicating results once all threads terminate.
