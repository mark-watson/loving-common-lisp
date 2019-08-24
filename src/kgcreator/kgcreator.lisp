;; KGCreator main program

(in-package #:kgcreator)

(defvar *base-pathname-pl* #.(or *compile-file-truename* *load-truename*))
(defvar a1 (write-to-string *base-pathname-pl*))
(defvar *current-directory-pl* (subseq a1 3 (- (length a1) 13)))

(ensure-directories-exist "temp/")

(defun get-files-and-meta (fpath)
  (let ((data (directory (concatenate 'string fpath "/" "*.txt")))
	      (meta (directory (concatenate 'string fpath "/" "*.meta"))))
    (if (not (equal (length data) (length meta)))
	(let ()
	  (princ "Error: must be matching *.meta files for each *.txt file")
	  (terpri)
	  '())
	(let ((ret '()))
	  (dotimes (i (length data))
	    (setq ret (cons (list (nth i data) (nth i meta)) ret)))
	  ret))))

(opts:define-opts
    (:name :help
	   :description "KGcreator command line app. example: ./KGcreator -i test_data -r out.rdf -c out.cyper"
	   :short #\h
	   :long "help")
    (:name :rdf
	   :description "RDF output file name"
	   :short #\r
	   :long "rdf"
	   :arg-parser #'identity ;; <- takes an argument
	   :arg-parser #'identity) ;; <- takes an argument
  (:name :cypher
	 :description "Cypher output file name"
	 :short #\c
	 :long "cypher"
	 :arg-parser #'identity) ;; <- takes an argument
  (:name :inputdir
	 :description "Cypher output file name"
	 :short #\i
	 :long "inputdir"
	 :arg-parser #'identity)) ;; <- takes an argument


(defun kgcreator () ;; don't need: &aux args sb-ext:*posix-argv*)
  (handler-case
      (let* ((opts (opts:get-opts))
	     (input-path
	      (if (find :inputdir opts)
		  (nth (1+ (position :inputdir opts)) opts)))
	     (rdf-output-path
	      (if (find :rdf opts)
		  (nth (1+ (position :rdf opts)) opts)))
	     (cypher-output-path
	      (if (find :cypher opts)
		  (nth (1+ (position :cypher opts)) opts))))
	(format t "input-path: ~a  rdf-output-path: ~a cypher-output-path:~a~%"
		input-path rdf-output-path cypher-output-path)
	(if (not input-path)
	    (format t "You must specify an input path.~%")
	    (locally
		(declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
	      (handler-bind
		  (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
		;; stuff that emits redefinition-warning's
		(let ()
		  (if rdf-output-path
		      (rdf-from-files rdf-output-path (get-files-and-meta input-path)))
		  (if cypher-output-path
		      (cypher-from-files cypher-output-path (get-files-and-meta input-path))))))))
    (t (c)
      (format t "We caught a runtime error: ~a~%" c)
      (values 0 c)))
  (format t "~%Shutting down KGcreator - done processing~%~%"))

(defun test1 ()
    (get-files-and-meta
     "~/GITHUB/common-lisp/kgcreator/test_data"))

(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

(defun test2 ()
  (let ((h (entities_dbpedia:find-entities-in-text "Bill Clinton and George Bush went to Mexico and England and watched Univision. They enjoyed Dakbayan sa Dabaw and shoped at Best Buy and listened to Al Stewart. They agree on República de Nicaragua and support Sweden Democrats and Leicestershire Miners Association and both sent their kids to Darul Uloom Deoband.")))
    (entities_dbpedia:entity-iterator #'print-hash-entry h)))

(defun test7 ()
  (rdf-from-files "out.rdf" (get-files-and-meta "test_data")))
