(in-package #:kgn)

(ql:quickload "sqlite")

(defvar *USE-CACHING* nil)

;;; command line arguments:

(defun command-line-args ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

;;; SqList caching for SPARQL queries:

(defvar *db-path* (pathname "~/Downloads/knowledge_graph_navigator_cache.db"))

(defun create-dbpedia ()
  (if *USE-CACHING*
      (sqlite:with-open-database (d *db-path*)
                                 (ignore-errors
                                   (sqlite:execute-single d "CREATE TABLE dbpedia (query string  PRIMARY KEY ASC, result string)")))))

(defun save-query-result-dbpedia (query result)
  (if *USE-CACHING*
      (let ()
        (ignore-errors (create-dbpedia))
        (sqlite:with-open-database (d *db-path*)
                                   (ignore-errors
                                     (sqlite:execute-to-list d
                                                             "insert into dbpedia (query, result) values (?, ?)"
                                                             query result))))))
(defun fetch-result-dbpedia (query)
  (if *USE-CACHING*
      (let ()
        (ignore-errors (create-dbpedia))
        (sqlite:with-open-database (d *db-path*)
                                   (cadar
                                    (sqlite:execute-to-list d
                                                            "select * from dbpedia where query = ?" query))))))

(defun create-wikidata ()
  (sqlite:with-open-database (d *db-path*)
    (ignore-errors
      (sqlite:execute-single d "CREATE TABLE wikidata (query string  PRIMARY KEY ASC, result string)"))))

(defun save-query-result-wikidata (query result)
  (ignore-errors (create-wikidata))
  (sqlite:with-open-database (d *db-path*)
    (ignore-errors
      (sqlite:execute-to-list d
                       "insert into wikidata (query, result) values (?, ?)"
                       query result))))
(defun fetch-result-wikidata (query)
  (ignore-errors (create-wikidata))
  (sqlite:with-open-database (d *db-path*)
    (cadar
     (sqlite:execute-to-list d
                      "select * from wikidata where query = ?" query))))


;; stop words/noise words removal

(defvar *stop-words* (make-hash-table :test #'equal :size 4000))
(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defvar a1 (write-to-string *base-pathname*))
(defvar *current-directory* (subseq a1 3 (- (length a1) 11)))

(let (line)
    (with-open-file
        (in (concatenate 'string *current-directory* "data/stopwords.txt"))
      (dotimes (i 50000)
        (setq line (read-line in nil nil))
        (if (null line) (return))
        (setf (gethash line *stop-words*) t))))

(defun remove-stop-words (word-list)
  (remove-if
   #'(lambda (x)
     (or
       (gethash (string-downcase x) *stop-words*)
       (let ((q nil))
         (ignore-errors
           (if (numberp (read-from-string x))
               (setf q t)))
         q)))
   word-list))


