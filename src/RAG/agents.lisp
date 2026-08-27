;;; agents.lisp — Multi-agent pipeline for Agentic RAG
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License
;;;
;;; Inspired by Google's "Unlocking Dependable Responses with Agentic RAG"
;;; https://research.google/blog/unlocking-dependable-responses-with-gemini-enterprise-agent-platforms-agentic-rag/
;;;
;;; Architecture:
;;;   1. Query Rewriter   — decomposes complex queries into focused sub-queries
;;;   2. Search Fanout    — searches corpora with embeddings for each sub-query
;;;   3. Sufficient Context Agent — evaluates if retrieved context is adequate
;;;   4. Synthesis Agent  — generates grounded answer from accumulated context

(in-package #:rag)

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

(defun %cap-context (scored-chunks max-context-chunks)
  "Keep at most MAX-CONTEXT-CHUNKS highest-scoring chunks so iterative
   retrieval cannot grow the LLM prompt without bound. SCORED-CHUNKS
   must already be sorted by descending score."
  (if (and max-context-chunks (> (length scored-chunks) max-context-chunks))
      (subseq scored-chunks 0 max-context-chunks)
      scored-chunks))

;;; ---- Agent 1: Query Rewriter ----

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
    ;; Always include the original query as a fallback
    (if queries
        queries
        (list user-query))))


;;; ---- Agent 2: Search Fanout ----

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
            ;; Deduplicate by chunk text
            (unless (gethash text seen-texts)
              (setf (gethash text seen-texts) t)
              (push result all-results))))))
    ;; Sort by score descending
    (let ((sorted (sort all-results #'> :key #'car)))
      (%debug-log "DEBUG search-fanout: found ~A unique chunks~%" (length sorted))
      sorted)))


;;; ---- Agent 3: Sufficient Context Agent ----

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


;;; ---- Agent 4: Synthesis Agent ----

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


;;; ---- Orchestrator: Agentic RAG Pipeline ----

(defun refine-queries (user-query feedback &key (model *rag-model*))
  "Generate refined search queries based on sufficiency FEEDBACK.
   Used when the initial retrieval was insufficient."
  (%debug-log "~%DEBUG refine-queries: generating refined queries from feedback~%")
  (let* ((prompt
           (format nil
                   "You are a search query rewriter. The previous search ~
                    did not find enough information. Based on the feedback ~
                    below, generate 1-2 NEW, DIFFERENT search queries to ~
                    find the missing information.~%~
                    ~%Original question: ~A~
                    ~%Missing information: ~A~
                    ~%~%Output ONLY the new queries, one per line. ~
                    No numbering or extra text."
                   user-query feedback))
         (queries (parse-query-lines (rag-generate prompt :model model))))
    (%debug-log "DEBUG refine-queries: generated ~A refined queries:~%~{  - ~A~%~}"
                (length queries) queries)
    (or queries (list feedback))))

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
        (%debug-log "DEBUG agentic-rag: no chunks found, returning empty answer~%")
        (return-from agentic-rag
          "I could not find any relevant information in the available documents."))

      ;; Cap prompt size regardless of how many iterations accumulated chunks
      (let ((context-chunks (%cap-context all-chunks max-context-chunks)))
        (multiple-value-bind (sufficient-p feedback)
            (assess-sufficiency user-query context-chunks :model model)

          (when sufficient-p
            (%debug-log "~%DEBUG agentic-rag: context is SUFFICIENT at iteration ~A~%"
                        iteration)
            ;; Phase 5: Synthesize answer
            (return-from agentic-rag
              (synthesize-answer user-query context-chunks :model model)))

          (when (>= iteration max-iterations)
            (%debug-log "~%DEBUG agentic-rag: max iterations reached, synthesizing ~
                         with available context~%")
            (return-from agentic-rag
              (synthesize-answer user-query context-chunks :model model)))

          ;; Phase 4: Refine and search again
          (%debug-log "~%DEBUG agentic-rag: context INSUFFICIENT, refining...~%")
          (%debug-log "DEBUG agentic-rag: feedback: ~A~%" feedback)
          (let* ((refined-queries (refine-queries user-query feedback :model model))
                 (new-chunks (search-fanout corpora refined-queries :top-k top-k)))
            ;; Accumulate new chunks with existing ones (deduplicate)
            (let ((seen (make-hash-table :test 'equal)))
              (dolist (scored-chunk all-chunks)
                (setf (gethash (document-chunk-text (cdr scored-chunk)) seen) t))
              (dolist (scored-chunk new-chunks)
                (unless (gethash (document-chunk-text (cdr scored-chunk)) seen)
                  (setf (gethash (document-chunk-text (cdr scored-chunk)) seen) t)
                  (push scored-chunk all-chunks))))
            ;; Re-sort by score
            (setf all-chunks (sort all-chunks #'> :key #'car))))))))
