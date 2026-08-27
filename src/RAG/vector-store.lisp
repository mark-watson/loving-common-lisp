;;; vector-store.lisp — In-memory vector store for document chunks
;;; Copyright (C) 2026 Mark Watson <markw@markwatson.com>
;;; Apache 2 License

(in-package #:rag)

;;; Simple in-memory vector store. Documents are chunked, embedded,
;;; and stored as lists. Retrieval uses brute-force cosine similarity.
;;; This is intentionally simple for clarity — production systems
;;; would use a dedicated vector database.

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

(defun corpus-chunk-count (corpus)
  "Number of document chunks currently stored in CORPUS."
  (length (corpus-chunks corpus)))

(defvar *default-chunk-size* 500
  "Default size in characters for splitting documents into chunks.")

(defvar *chunk-overlap* 50
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

(defun add-document (corpus filepath &key (chunk-size *default-chunk-size*))
  "Read a text file, split it into chunks, compute embeddings (in a
   single batch API call when the default embedding function is used),
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
                       collect (make-document-chunk :text chunk-text
                                                    :source source
                                                    :embedding embedding))))
    (%debug-log "DEBUG add-document: added ~A chunks from ~A~%"
                (length chunks) source)
    (length chunks)))

;;; ---- Corpus persistence ----

(defun save-corpus (corpus pathname)
  "Write CORPUS (name, description, chunks with embeddings) to PATHNAME
   as a single s-expression. Load it back with LOAD-CORPUS. Saving lets
   you avoid re-embedding documents (and re-paying API calls) each run."
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
  "Load a corpus previously written by SAVE-CORPUS. Returns a corpus struct."
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

;;; ---- Retrieval ----

(defun search-corpus (corpus query-embedding &key (top-k 3))
  "Search CORPUS for the TOP-K chunks most similar to QUERY-EMBEDDING.
   Returns a list of (score . document-chunk) pairs, sorted by
   descending similarity."
  (let ((scored-chunks
          (loop for chunk in (corpus-chunks corpus)
                collect (cons (cosine-similarity query-embedding
                                                 (document-chunk-embedding chunk))
                              chunk))))
    ;; Sort by score descending and take top-k
    (subseq (sort scored-chunks #'> :key #'car)
            0 (min top-k (length scored-chunks)))))

(defun search-corpora (corpora query-embedding &key (top-k 3))
  "Search multiple CORPORA for the TOP-K most similar chunks overall.
   Returns a list of (score . document-chunk) pairs."
  (let ((all-results
          (loop for corpus in corpora
                append (search-corpus corpus query-embedding
                                      :top-k top-k))))
    ;; Re-sort combined results and take top-k
    (subseq (sort all-results #'> :key #'car)
            0 (min top-k (length all-results)))))

(defun format-retrieved-chunks (scored-chunks)
  "Format scored chunks into a text string for use as LLM context.
   Each chunk is labeled with its source and similarity score."
  (with-output-to-string (out)
    (loop for (score . chunk) in scored-chunks
          for i from 1
          do (format out "~%--- Retrieved Passage ~A (source: ~A, relevance: ~,2F) ---~%~A~%"
                     i
                     (document-chunk-source chunk)
                     score
                     (document-chunk-text chunk)))))
