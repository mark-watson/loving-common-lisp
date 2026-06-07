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

(defvar *default-chunk-size* 500
  "Default size in characters for splitting documents into chunks.")

(defvar *chunk-overlap* 50
  "Number of characters to overlap between adjacent chunks.")

(defun read-file-contents (filepath)
  "Read the entire contents of FILEPATH as a string."
  (with-open-file (stream filepath :direction :input
                                    :if-does-not-exist :error)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      ;; Trim any trailing null characters from the buffer
      (string-right-trim '(#\Null) contents))))

(defun split-into-chunks (text &key (chunk-size *default-chunk-size*)
                                    (overlap *chunk-overlap*))
  "Split TEXT into overlapping chunks of approximately CHUNK-SIZE characters.
   Tries to break at sentence boundaries when possible."
  (let ((chunks nil)
        (len (length text))
        (start 0))
    (loop while (< start len)
          do (let* ((end (min (+ start chunk-size) len))
                    ;; Try to find a sentence boundary near the end
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
                                    end))
                    (chunk (string-trim '(#\Space #\Newline #\Tab)
                                        (subseq text start actual-end))))
               (when (> (length chunk) 0)
                 (push chunk chunks))
               ;; If we've reached the end of text, stop
               (if (>= actual-end len)
                   (setf start len)
                   (setf start (- actual-end overlap)))))
    (nreverse chunks)))

(defun add-document (corpus filepath &key (chunk-size *default-chunk-size*))
  "Read a text file, split it into chunks, compute embeddings,
   and add the chunks to CORPUS. Returns the number of chunks added."
  (format t "~%DEBUG add-document: loading ~A~%" filepath)
  (let* ((text (read-file-contents filepath))
         (chunks (split-into-chunks text :chunk-size chunk-size))
         (source (file-namestring filepath))
         (count 0))
    (format t "DEBUG add-document: split into ~A chunks~%" (length chunks))
    (dolist (chunk-text chunks)
      (let ((embedding (get-embedding chunk-text)))
        (push (make-document-chunk :text chunk-text
                                   :source source
                                   :embedding embedding)
              (corpus-chunks corpus))
        (incf count)))
    (format t "DEBUG add-document: added ~A chunks from ~A~%" count source)
    count))

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
