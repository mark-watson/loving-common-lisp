;;; vector-store.lisp — In-memory vector store for document chunks
;;; Copyright (C) 2026 Mark Watson <markwatson@markwatson.com>
;;; Apache 2 License

(in-package #:rag)

;;; Simple in-memory vector store. Documents are chunked, embedded,
;;; and stored as normalized simple-vectors with precomputed norms.
;;; Retrieval uses brute-force cosine similarity. This is intentionally
;;; simple for clarity — production systems would use a dedicated
;;; vector database.

(defun %print-embedding-preview (vector stream)
  "Print up to 10 leading values of VECTOR plus a dimension count."
  (format stream "#(")
  (loop for i below (min 10 (length vector))
        do (format stream "~:[~; ~]~A" (> i 0) (aref vector i)))
  (when (> (length vector) 10)
    (format stream " ..."))
  (format stream ") [~A dimensions]" (length vector)))

(defun %print-document-chunk (chunk stream depth)
  "Custom printer for document-chunk: text and source in full, but only
    the first 10 embedding values, so REPL inspection of corpora does not
    dump thousands of floats. Unreadable (#<) on purpose: a readable #S
    form with a truncated embedding could be read back as real data."
  (declare (ignore depth))
  (format stream "#<DOCUMENT-CHUNK :SOURCE ~S :TEXT ~S :EMBEDDING "
          (document-chunk-source chunk)
          (document-chunk-text chunk))
  (%print-embedding-preview (document-chunk-embedding chunk) stream)
  (format stream ">"))

(defstruct (document-chunk (:print-function %print-document-chunk))
  "A chunk of text with its source file, embedding vector (a normalized
    simple-vector), and precomputed L2 norm (1.0 for normalized chunks)."
  text
  source
  embedding
  (norm 1.0 :type float))

(defun document-chunk-key (chunk)
  "Stable identity for a chunk across corpora: (source . text). The
    same text in different source files is a different chunk."
  (cons (document-chunk-source chunk) (document-chunk-text chunk)))

(defun %print-corpus (corpus stream depth)
  "Custom printer for corpus: name, description, and chunk count instead
    of every chunk. The chunks are reachable with (corpus-chunks c)."
  (declare (ignore depth))
  (format stream "#<CORPUS :NAME ~S :DESCRIPTION ~S :CHUNKS ~A>"
          (corpus-name corpus)
          (corpus-description corpus)
          (length (corpus-chunks corpus))))

(defstruct (corpus (:print-function %print-corpus))
  "A named collection of document chunks for retrieval."
  name
  description
  (chunks nil))

(defun corpus-chunk-count (corpus)
  "Number of document chunks currently stored in CORPUS."
  (length (corpus-chunks corpus)))

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

(defun make-document-chunk/embedded (text source raw-embedding)
  "Build a document-chunk with a normalized simple-vector embedding and
    its precomputed norm. All chunks in the store are normalized, so
    search is a dot product (see search-corpus)."
  (let* ((vec (coerce raw-embedding 'simple-vector))
         (norm (vector-magnitude vec)))
    (when (zerop norm)
      (error "Zero-magnitude embedding for chunk from ~A; cannot normalize" source))
    (make-document-chunk :text text :source source :embedding vec :norm norm)))

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

;;; ---- Retrieval ----

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