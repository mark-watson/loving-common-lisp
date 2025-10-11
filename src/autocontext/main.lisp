;; main.lisp
;; The core AutoContext class and application logic.

(defpackage #:autocontext
  (:use #:cl #:bm25)
  (:export #:auto-context
           #:get-prompt
           #:run-example))

(in-package #:autocontext)

(defclass auto-context ()
  ((chunks :reader chunks :initarg :chunks :documentation "A list of original text chunks.")
   (bm25-index :reader bm25-index :initarg :bm25-index :documentation "The BM25 sparse index.")
   (chunk-embeddings :reader chunk-embeddings :initarg :chunk-embeddings :documentation "A magicl matrix of dense embeddings.")))

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

(defun list-txt-files-in-directory (dir-path)
  "Return a list of pathnames for *.txt files in the directory given by DIR-PATH (a string or pathname)."
  (let* ((base (uiop:parse-native-namestring dir-path))
         ;; ensure base is a directory pathname
         (dir (uiop:ensure-directory-pathname base))
         ;; create a wildcard pathname for *.txt under that directory
         (pattern (uiop:merge-pathnames* 
                    (make-pathname :name :wild :type "txt")
                    dir)))
    (directory pattern)))


(defun split-into-sentences (text)
  "Splits text into a list of sentences based on punctuation. This is a heuristic approach and may not be perfect. It tries to avoid splitting on abbreviations like 'e.g.'."
  (let ((sentences '())
        (start 0))
    (loop for i from 0 below (length text)
          do (when (and (member (char text i) '(#\. #\? #\!))
                        (or (= (1+ i) (length text))
                            (member (char text (1+ i)) '(#\Space #\Newline #\"))))
               (push (string-trim '(#\Space #\Newline) (subseq text start (1+ i))) sentences)
               (setf start (1+ i))))
    (let ((last-part (string-trim '(#\Space #\Newline) (subseq text start))))
      (when (plusp (length last-part))
        (push last-part sentences)))
    (remove-if (lambda (s) (zerop (length s))) (nreverse sentences))))

(defun chunk-text (text &key (chunk-size 3))
  "Splits text into sentences and then groups them into chunks of chunk-size sentences."
  (let ((sentences (split-into-sentences text)))
    (loop for i from 0 below (length sentences) by chunk-size
          collect (let* ((end (min (+ i chunk-size) (length sentences)))
                         (sentence-group (subseq sentences i end)))
                    (string-trim '(#\Space #\Newline) (format nil "~{~a~^ ~}" sentence-group))))))

(defun load-and-chunk-documents (directory-path)
  "Loads .txt files and splits them into chunks of a few sentences."
  (format t "&load-and-chunk-documents: directory-path=~A~%" directory-path)
  (let* ((chunks (remove-if (lambda (s) (string= s ""))
                            (loop for file in (list-txt-files-in-directory directory-path)
                                  nconcing (chunk-text (uiop:read-file-string file))))))
    (format t "~&Loaded ~d text chunks." (length chunks))
    ;;(format t "&load-and-chunk-documents: chunks=~A~%~%" chunks)
    chunks));;; Embedding Generation (Interface to Python)

(defun generate-embeddings (text-list)
  "Calls the Python script to generate embeddings for a list of strings."
  (let* ((input-string (format nil "~{~a~%~}" text-list))
         (command "uv run generate_embeddings.py")
         (json-output (uiop:run-program command
                                        :input (make-string-input-stream input-string)
                                        :output :string)))
    (let* ((parsed (yason:parse json-output))
           (num-embeddings (length parsed))
           (embedding-dim (if (> num-embeddings 0) (length (first parsed)) 0))
           (flat-data (apply #'append parsed)))
      (magicl:from-list flat-data (list num-embeddings embedding-dim) :type 'double-float))))

;;; Core Method

(defun cosine-similarity (vec1 vec2)
  "Calculates cosine similarity between two magicl vectors."
  (/ (magicl:dot vec1 vec2)
     (* (magicl:norm vec1) (magicl:norm vec2))))

(defun get-row-vector (matrix row-index)
  "Extracts a row from a matrix and returns it as a magicl vector."
  (let* ((num-cols (magicl:ncols matrix))
         (row-elements (loop for col-index from 0 below num-cols
                             collect (magicl:tref matrix row-index col-index))))
    (magicl:from-list row-elements (list num-cols) :type (magicl:element-type matrix))))

(defmethod get-prompt ((ac auto-context) query &key (num-results 5))
  "Retrieves context and formats it into a prompt for an LLM."
  (format t "~&--- Retrieving context for query: '~a' ---" query)

  ;; 1. Sparse Search (BM25)
  (let* ((query-tokens (tokenize query))
         (bm25-docs (bm25:get-top-n (bm25-index ac) query-tokens num-results))
         (bm25-results (mapcar (lambda (tokens) (format nil "~{~a~^ ~}" tokens)) bm25-docs)))
    (format t "~&BM25 found ~d keyword-based results." (length bm25-results))
    (format t "~%~%bm25-results:~%~A~%~%" bm25-results)

     ;; 2. Dense Search (Vector Similarity)
     (let* ((query-embedding-matrix (generate-embeddings (list query)))
            (query-vector (get-row-vector query-embedding-matrix 0))
            (all-embeddings (chunk-embeddings ac))
            (similarities (loop for i from 0 below (magicl:nrows all-embeddings)
                                collect (cons (cosine-similarity query-vector
                                                                 (get-row-vector all-embeddings i))
                                              i)))
            (sorted-sim (sort similarities #'> :key #'car))
            (top-indices (mapcar #'cdr (subseq sorted-sim 0 (min num-results (length sorted-sim)))))
            (vector-results (mapcar (lambda (i) (nth i (chunks ac))) top-indices)))
       (format t "~&Vector search found ~d semantic-based results." (length vector-results))
       (format t "~%~%vector-results:~%~A~%~%" vector-results)

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
         (query "who says that economics is bullshit?")
         (prompt (get-prompt ac query :num-results 2)))
      (format t "~&~%--- Generated Prompt for LLM ---~%~a" prompt)))
