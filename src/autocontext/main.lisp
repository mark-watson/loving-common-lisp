;; main.lisp
;; The core AutoContext class and application logic.

(defpackage #:autocontext
  (:use #:cl #:bm25)
  (:export #:auto-context
           #:get-prompt
           #:run-example))

(in-package #:autocontext)

(defclass auto-context ()
  ((chunks :reader chunks :documentation "A list of original text chunks.")
   (bm25-index :reader bm25-index :documentation "The BM25 sparse index.")
   (chunk-embeddings :reader chunk-embeddings :documentation "A magicl matrix of dense embeddings.")))

;;; Initialization

(defmethod initialize-instance :after ((ac auto-context) &key directory-path)
  "Constructor for the auto-context class. Loads data and builds indices."
  (format t "~&Initializing AutoContext from directory: ~a" directory-path)
  (let ((chunks (load-and-chunk-documents directory-path)))
    (when chunks
      (format t "~&Building sparse and dense retrievers...")
      (let* ((tokenized-chunks (mapcar #'tokenize chunks))
             (bm25 (make-bm25-index tokenized-chunks))
             (embeddings (generate-embeddings chunks)))
        ;; Using shared-initialize to set the slots after creation
        (shared-initialize ac t :chunks chunks
                               :bm25-index bm25
                               :chunk-embeddings embeddings))
      (format t "~&Initialization complete. AutoContext is ready."))))

(defun tokenize (text)
  "Simple whitespace tokenizer."
  (split-sequence:split-sequence #\Space text :remove-empty-subseqs t))

(defun load-and-chunk-documents (directory-path)
  "Loads .txt files and splits them into paragraph chunks."
  (let ((chunks ()))
    (unless (uiop:directory-exists-p directory-path)
      (error "Directory not found: ~a" directory-path))
    (dolist (file (uiop:directory-files directory-path #P"*.txt"))
      (let* ((content (uiop:read-file-string file))
             (paragraphs (split-sequence:split-sequence #\Newline content
                                                        :count 2
                                                        :remove-empty-subseqs t)))
        (setf chunks (nconc chunks (mapcar (lambda (p) (string-trim '(#\Space #\Newline) p)) paragraphs)))))
    (format t "~&Loaded ~d text chunks." (length chunks))
    chunks))

;;; Embedding Generation (Interface to Python)

(defun generate-embeddings (text-list)
  "Calls the Python script to generate embeddings for a list of strings."
  (let* ((input-string (format nil "~{~a~%~}" text-list))
         (command "uv run generate_embeddings.py")
         (json-output (uiop:run-program command
                                        :input (make-string-input-stream input-string)
                                        :output :string)))
    (let ((parsed (yason:parse json-output)))
      (pprint parsed)
      (magicl:from-list parsed (list (length parsed) (length (first parsed))) :type 'double-float))))

;;; Core Method

(defun cosine-similarity (vec1 vec2)
  "Calculates cosine similarity between two magicl vectors."
  (/ (magicl:dot vec1 vec2)
     (* (magicl:norm vec1) (magicl:norm vec2))))

(defmethod get-prompt ((ac auto-context) query &key (num-results 5))
  "Retrieves context and formats it into a prompt for an LLM."
  (format t "~&--- Retrieving context for query: '~a' ---" query)

  ;; 1. Sparse Search (BM25)
  (let* ((query-tokens (tokenize query))
         (bm25-docs (bm25:get-top-n (bm25-index ac) query-tokens num-results))
         (bm25-results (mapcar (lambda (tokens) (format nil "~{~a~^ ~}" tokens)) bm25-docs)))
     (format t "~&BM25 found ~d keyword-based results." (length bm25-results))

     ;; 2. Dense Search (Vector Similarity)
     (let* ((query-embedding-matrix (generate-embeddings (list query)))
            (query-vector (magicl:slice query-embedding-matrix 0 0 :n2 (magicl:ncols query-embedding-matrix)))
            (all-embeddings (chunk-embeddings ac))
            (similarities (loop for i from 0 below (magicl:nrows all-embeddings)
                                collect (cons (cosine-similarity query-vector
                                                                 (magicl:slice all-embeddings i 0 :n2 (magicl:ncols all-embeddings)))
                                              i)))
            (sorted-sim (sort similarities #'> :key #'car))
            (top-indices (mapcar #'cdr (subseq sorted-sim 0 (min num-results (length sorted-sim)))))
            (vector-results (mapcar (lambda (i) (nth i (chunks ac))) top-indices)))
        (format t "~&Vector search found ~d semantic-based results." (length vector-results))

       ;; 3. Combine and deduplicate
       (let* ((combined (append bm25-results vector-results))
              (unique-results (remove-duplicates combined :test #'string= :from-end t)))
         (format t "~&Combined and deduplicated, we have ~d context chunks." (length unique-results))

         ;; 4. Format the final prompt
         (format nil "Based on the following context, please answer the question.~2%--- CONTEXT ---~%~{~a~%~%---~%~%~}--- END CONTEXT ---~2%Question: ~a~%Answer:"
                 unique-results query)))))

;;; Example Usage


(defun test2 ()
  "A simple top-level function to demonstrate the system."
  (let* ((ac (make-instance 'auto-context :directory-path "../data"))
         (query "who was the first person to walk on the lunar surface?")
         (prompt (get-prompt ac query :num-results 2)))
      (format t "~&~%--- Generated Prompt for LLM ---~%~a" prompt)))
