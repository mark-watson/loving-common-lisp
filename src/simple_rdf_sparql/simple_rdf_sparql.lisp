(in-package #:simple_rdf_sparql)

;; Copyright 2024 Mark Watson. All rights reserved. License: AGPL-v3

;; RDF triple structure
(defstruct triple
  subject
  predicate
  object)

;; RDF datastore
(defvar *rdf-store* nil)

;; Add a triple to the datastore
(defun add-triple (subject predicate object)
  (push (make-triple :subject subject :predicate predicate :object object)
        *rdf-store*))

;; Remove a triple from the datastore
(defun remove-triple (subject predicate object)
  (setf *rdf-store*
        (remove-if (lambda (triple)
                     (and (equal (triple-subject triple) subject)
                          (equal (triple-predicate triple) predicate)
                          (equal (triple-object triple) object)))
                   *rdf-store*)))

;; Helper function to check if a string is a variable
(defun variable-p (str)
  (and (stringp str) (> (length str) 0) (char= (char str 0) #\?)))

;; Convert triple to binding
(defun triple-to-binding (triple &optional pattern)
  (let ((binding nil))
    (when (and pattern (variable-p (first pattern)))
      (push (cons (first pattern) (triple-subject triple)) binding))
    (when (and pattern (variable-p (second pattern)))
      (push (cons (second pattern) (triple-predicate triple)) binding))
    (when (and pattern (variable-p (third pattern)))
      (push (cons (third pattern) (triple-object triple)) binding))
    binding))

(defun query-triples (subject predicate object)
  (remove-if-not (lambda (triple)
                   (and (or (null subject) (variable-p subject) (equal (triple-subject triple) subject))
                        (or (null predicate) (variable-p predicate) (equal (triple-predicate triple) predicate))
                        (or (null object) (variable-p object) (equal (triple-object triple) object))))
                 *rdf-store*))

;; Print all triples in the datastore
(defun print-all-triples ()
  (format t "All triples in the datastore:~%")
  (dolist (triple *rdf-store*)
    (format t "~A ~A ~A~%"
            (triple-subject triple)
            (triple-predicate triple)
            (triple-object triple)))
  (format t "~%"))

;; SPARQL query structure
(defstruct sparql-query
  select-vars
  where-patterns)

;; Simple string splitting function
(defun split-string (string &optional (delimiter #\Space))
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))


;; Apply bindings to a pattern
(defun apply-bindings (pattern bindings)
  (mapcar (lambda (item)
            (if (variable-p item)
                (or (cdr (assoc item bindings :test #'string=)) item)
                item))
          pattern))

;; Merge bindings
(defun merge-bindings (binding1 binding2)
  (append binding1 binding2))

(defun parse-where-patterns (where-clause)
  (loop with patterns = nil
        with current-pattern = nil
        for token in where-clause
        do (cond
             ((string= token ".")
              (when current-pattern
                (push (reverse current-pattern) patterns)
                (setf current-pattern nil)))
             (t
              (push token current-pattern)))
        finally (when current-pattern
                  (push (reverse current-pattern) patterns))
                (return (reverse patterns))))

(defun parse-sparql-query (query-string)
  (let* ((tokens (remove-if (lambda (token) (member token '("{" "}") :test #'string=))
                            (split-string query-string)))
         (select-index (position "select" tokens :test #'string-equal))
         (where-index (position "where" tokens :test #'string-equal))
         (select-vars (subseq tokens (1+ select-index) where-index))
         (where-clause (subseq tokens (1+ where-index)))
         (where-patterns (parse-where-patterns where-clause)))
    (make-sparql-query :select-vars select-vars
                       :where-patterns where-patterns)))


(defun remove-duplicate-bindings (bindings)
  (remove-duplicates bindings :test #'equal :key #'car))

(defun project-results (results select-vars)
  (if (equal select-vars '("*"))
      (mapcar #'remove-duplicate-bindings results)
      (mapcar (lambda (result)
                (remove-duplicate-bindings
                 (mapcar (lambda (var)
                           (cons var (cdr (assoc var result :test #'string=))))
                         select-vars)))
              results)))

;; Execute WHERE patterns with bindings
(defun execute-where-patterns-with-bindings (patterns bindings)
  (if (null patterns)
      (list bindings)
      (let* ((pattern (first patterns))
             (remaining-patterns (rest patterns))
             (bound-pattern (apply-bindings pattern bindings))
             (matching-triples (apply #'query-triples bound-pattern)))
        (let ((new-bindings (mapcar (lambda (triple)
                                      (merge-bindings bindings (triple-to-binding triple pattern)))
                                    matching-triples)))
          (if (null remaining-patterns)
              new-bindings
              (mapcan (lambda (binding)
                        (execute-where-patterns-with-bindings remaining-patterns binding))
                      new-bindings))))))

(defun execute-where-patterns (patterns)
  ;;(format t "Executing where patterns: ~A~%" patterns)
  (if (null patterns)
      (list nil)
      (let* ((pattern (first patterns))
             (remaining-patterns (rest patterns))
             (matching-triples (apply #'query-triples pattern)))
        ;;(format t "Matching triples for pattern ~A: ~A~%" pattern matching-triples)
        (let ((bindings (mapcar (lambda (triple) (triple-to-binding triple pattern)) matching-triples)))
          ;;(format t "Initial bindings: ~A~%" bindings)
          (if (null remaining-patterns)
              bindings
              (mapcan (lambda (binding)
                        (let ((results (execute-where-patterns-with-bindings remaining-patterns binding)))
                          (mapcar (lambda (result)
                                    (merge-bindings binding result))
                                  results)))
                      bindings))))))


(defun execute-sparql-query (query-string)
  (let* ((query (parse-sparql-query query-string))
         (where-patterns (sparql-query-where-patterns query))
         (select-vars (sparql-query-select-vars query))
         (results (execute-where-patterns where-patterns))
         (projected-results (project-results results select-vars)))
    ;;(format t "Parsed query: ~A~%" query)
    ;;(format t "Where patterns: ~A~%" where-patterns)
    ;;(format t "Select vars: ~A~%" select-vars)
    ;;(format t "Results before projection: ~A~%" results)
    projected-results))

(defun test ()
  (setf *rdf-store* nil)

  (add-triple "John" "age" "30")
  (add-triple "John" "likes" "pizza")
  (add-triple "Mary" "age" "25")
  (add-triple "Mary" "likes" "sushi")
  (add-triple "Bob" "age" "35")
  (add-triple "Bob" "likes" "burger")

  (print-all-triples)

  (defun print-query-results (query-string)
    (format t "Query: ~A~%" query-string)
    (let ((results (execute-sparql-query query-string)))
      (format t "Final Results:~%")
      (if results
          (dolist (result results)
            (format t "  ~{~A: ~A~^, ~}~%"
                    (loop for (var . value) in result
                          collect var collect value)))
          (format t "  No results~%"))
      (format t "~%")))

  (print-query-results "select * where { ?name age ?age . ?name likes ?food }")
  (print-query-results "select ?s ?o where { ?s likes ?o }")
  (print-query-results "select * where { ?name age ?age . ?name likes pizza }"))


;; Run the main function
;;(simple_rdf_sparql:test)
