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

(defvar *rag-model* "gemini-2.0-flash"
  "Inexpensive Gemini model used for all agent LLM calls.")

(defvar *interactions-api-url*
  "https://generativelanguage.googleapis.com/v1beta/interactions")

(defun rag-generate (prompt &key (model-id *rag-model*))
  "Call the Gemini Interactions API via curl (bypasses Dexador).
   Returns the generated text string."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "model" payload) model-id
          (gethash "input" payload) prompt)
    (let* ((payload-json (cl-json:encode-json-to-string payload))
           (api-key (get-google-api-key))
           (curl-command
             (list "curl" "-s" "-X" "POST"
                   "-H" "Content-Type: application/json"
                   "-H" (concatenate 'string "x-goog-api-key: " api-key)
                   "-H" "Api-Revision: 2026-05-20"
                   "-d" payload-json
                   *interactions-api-url*))
           (response-string
             (uiop:run-program curl-command
                               :output :string
                               :error-output :string
                               :ignore-error-status t))
           (decoded (cl-json:decode-json-from-string response-string))
           (steps (cdr (assoc :STEPS decoded))))
      ;; Extract text from last model_output step
      (loop for step in (reverse steps)
            when (string-equal (cdr (assoc :TYPE step)) "model_output")
            return (let* ((content (cdr (assoc :CONTENT step)))
                          (first-content (first content)))
                     (cdr (assoc :TEXT first-content)))))))

;;; ---- Agent 1: Query Rewriter ----

(defun rewrite-queries (user-query)
  "Decompose USER-QUERY into 1-3 focused sub-queries for retrieval.
   Returns a list of query strings."
  (format t "~%DEBUG rewrite-queries: decomposing query...~%")
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
         (response (rag-generate prompt))
         (queries (remove-if (lambda (s) (zerop (length s)))
                             (mapcar (lambda (line)
                                       (string-trim '(#\Space #\Tab #\- #\* #\1 #\2 #\3 #\.)
                                                    line))
                                     (uiop:split-string response
                                                        :separator '(#\Newline))))))
    (format t "DEBUG rewrite-queries: generated ~A sub-queries:~%~{  - ~A~%~}"
            (length queries) queries)
    ;; Always include the original query as a fallback
    (if queries
        queries
        (list user-query))))


;;; ---- Agent 2: Search Fanout ----

(defun search-fanout (corpora sub-queries &key (top-k 3))
  "Execute embedding search across CORPORA for each sub-query.
   Returns a deduplicated list of (score . document-chunk) pairs."
  (format t "~%DEBUG search-fanout: searching ~A corpora with ~A queries~%"
          (length corpora) (length sub-queries))
  (let ((all-results nil)
        (seen-texts (make-hash-table :test 'equal)))
    (dolist (query sub-queries)
      (format t "DEBUG search-fanout: embedding query: ~S~%" query)
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
      (format t "DEBUG search-fanout: found ~A unique chunks~%" (length sorted))
      sorted)))


;;; ---- Agent 3: Sufficient Context Agent ----

(defun assess-sufficiency (user-query retrieved-chunks)
  "Evaluate whether RETRIEVED-CHUNKS provide sufficient context
   to answer USER-QUERY. Returns two values:
     1. SUFFICIENT-P — T if context is sufficient, NIL otherwise
     2. FEEDBACK — String describing what information is missing."
  (format t "~%DEBUG assess-sufficiency: evaluating ~A chunks~%"
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
         (response (rag-generate prompt)))
    (format t "DEBUG assess-sufficiency response:~%~A~%" response)
    ;; Parse the response
    (let* ((verdict-line (find-if (lambda (line)
                                    (search "VERDICT:" line :test #'char-equal))
                                  (uiop:split-string response
                                                     :separator '(#\Newline))))
           (sufficient-p (and verdict-line
                             (search "SUFFICIENT" verdict-line :test #'char-equal)
                             (not (search "INSUFFICIENT" verdict-line
                                          :test #'char-equal))))
           ;; Extract the MISSING feedback for refinement
           (missing-line (find-if (lambda (line)
                                    (search "MISSING:" line :test #'char-equal))
                                  (uiop:split-string response
                                                     :separator '(#\Newline))))
           (feedback (if missing-line
                         (string-trim '(#\Space)
                                      (subseq missing-line
                                              (+ (search "MISSING:" missing-line
                                                         :test #'char-equal)
                                                 8)))
                         "No specific feedback available")))
      (format t "DEBUG assess-sufficiency: verdict=~A~%" 
              (if sufficient-p "SUFFICIENT" "INSUFFICIENT"))
      (values sufficient-p feedback))))


;;; ---- Agent 4: Synthesis Agent ----

(defun synthesize-answer (user-query retrieved-chunks)
  "Generate a grounded answer to USER-QUERY using RETRIEVED-CHUNKS.
   The answer cites source documents."
  (format t "~%DEBUG synthesize-answer: generating answer from ~A chunks~%"
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
         (response (rag-generate prompt)))
    (format t "DEBUG synthesize-answer: generated response (~A chars)~%"
            (length response))
    response))


;;; ---- Orchestrator: Agentic RAG Pipeline ----

(defun refine-queries (user-query feedback)
  "Generate refined search queries based on sufficiency FEEDBACK.
   Used when the initial retrieval was insufficient."
  (format t "~%DEBUG refine-queries: generating refined queries from feedback~%")
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
         (response (rag-generate prompt))
         (queries (remove-if (lambda (s) (zerop (length s)))
                             (mapcar (lambda (line)
                                       (string-trim '(#\Space #\Tab #\- #\* #\1 #\2 #\3 #\.)
                                                    line))
                                     (uiop:split-string response
                                                        :separator '(#\Newline))))))
    (format t "DEBUG refine-queries: generated ~A refined queries:~%~{  - ~A~%~}"
            (length queries) queries)
    (or queries (list feedback))))

(defun agentic-rag (corpora user-query &key (max-iterations 3) (top-k 3))
  "Run the full agentic RAG pipeline:
     1. Rewrite the user query into sub-queries
     2. Search corpora for relevant chunks
     3. Check if context is sufficient (loop if not)
     4. Synthesize a grounded answer

   CORPORA is a list of corpus structs.
   Returns the synthesized answer string."
  (format t "~%~%========================================~%")
  (format t "  AGENTIC RAG PIPELINE~%")
  (format t "  Query: ~A~%" user-query)
  (format t "========================================~%")
  
  ;; Phase 1: Rewrite queries
  (let* ((sub-queries (rewrite-queries user-query))
         ;; Phase 2: Initial search
         (all-chunks (search-fanout corpora sub-queries :top-k top-k))
         (iteration 0))
    
    ;; Phase 3: Iterative sufficiency check
    (loop
      (incf iteration)
      (format t "~%--- Iteration ~A/~A ---~%" iteration max-iterations)
      
      (when (null all-chunks)
        (format t "DEBUG agentic-rag: no chunks found, returning empty answer~%")
        (return-from agentic-rag
          "I could not find any relevant information in the available documents."))
      
      (multiple-value-bind (sufficient-p feedback)
          (assess-sufficiency user-query all-chunks)
        
        (when sufficient-p
          (format t "~%DEBUG agentic-rag: context is SUFFICIENT at iteration ~A~%"
                  iteration)
          ;; Phase 5: Synthesize answer
          (return-from agentic-rag
            (synthesize-answer user-query all-chunks)))
        
        (when (>= iteration max-iterations)
          (format t "~%DEBUG agentic-rag: max iterations reached, synthesizing ~
                     with available context~%")
          (return-from agentic-rag
            (synthesize-answer user-query all-chunks)))
        
        ;; Phase 4: Refine and search again
        (format t "~%DEBUG agentic-rag: context INSUFFICIENT, refining...~%")
        (format t "DEBUG agentic-rag: feedback: ~A~%" feedback)
        (let* ((refined-queries (refine-queries user-query feedback))
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
          (setf all-chunks (sort all-chunks #'> :key #'car)))))))
