;; Misc. plotting examples using the vecto library

(in-package #:kgcreator)

(let ((*entity-nodes-hash*))
  
  (defun cypher-from-files (output-file-path text-and-meta-pairs)
    (setf *entity-nodes-hash* (make-hash-table :test #'equal :size 200))
    ;;(print (list "==> cypher-from-files"output-file-path text-and-meta-pairs ))
    (with-open-file
	(str output-file-path
	     :direction :output
	     :if-exists :supersede   ;; WARNING: after I start testing with more than  file, need to change this
	     :if-does-not-exist :create)

      (defun generateNeo4jCategoryNodes ()
	(let* ((names categorize_summarize::categoryNames))
	  (dolist (name names)
	    (format str
		    (myutils:replace-all
		     (concatenate
		      'string "CREATE (" name ":CategoryType {name:\"" name "\"})~%")
		     "/" "_"))))
	(format str "~%"))
      

      (defun cypher-from-files-handle-single-file (text-input-file meta-input-file)
	(let* ((text (file-to-string text-input-file))
	       (words (myutils:words-from-string text))
	       (meta (file-to-string meta-input-file))) ;; TBD parse JSON
	  
	  (defun generate-original-doc-node ()
	    (let ((node-name (node-name-from-uri meta)))
	      (if (null (gethash node-name *entity-nodes-hash*))
		  (let* ((cats (categorize words))
			 (sum (summarize words cats)))
		    (setf (gethash node-name *entity-nodes-hash*) t)
		    (format str (concatenate 'string "CREATE (" node-name ":News {name:\""
					     node-name "\", uri: \"" meta "\", summary: \"" sum "\"})~%"))
		    (dolist (cat cats)
		      (let ((hash-check (concatenate 'string node-name (car cat))))
			(if (null (gethash hash-check *entity-nodes-hash*))
			    (let ()
			      (setf (gethash hash-check *entity-nodes-hash*) t)
			      (format str (concatenate 'string "CREATE (" node-name ")-[:Category]->("
						       (car cat) ")~%"))))))))))
	  
	  (defun generate-dbpedia-nodes (key entity-pairs)
	    (dolist (entity-pair entity-pairs)
	      (if (null (gethash (node-name-from-uri (cadr entity-pair)) *entity-nodes-hash*))
		  (let ()
		    (setf (gethash (node-name-from-uri (cadr entity-pair)) *entity-nodes-hash*) t)
		    (format str
			    (concatenate 'string "CREATE (" (node-name-from-uri (cadr entity-pair)) ":"
					 key " {name: \"" (car entity-pair)
					 "\", uri: \"" (cadr entity-pair) "\"})~%"))))))
	  
	  (defun generate-dbpedia-contains-cypher (key value)
	    (generate-original-doc-node)
	    (generate-dbpedia-nodes key value)
	    (let ((relation-name (concatenate 'string key "DbPediaLink")))
	      (dolist (entity-pair value)
		(let* ((node-name (node-name-from-uri meta))
		       (object-node-name (node-name-from-uri (cadr entity-pair)))
		       (hash-check (concatenate 'string node-name object-node-name)))
		  (if (null (gethash hash-check *entity-nodes-hash*))
		      (let ()
			(setf (gethash hash-check *entity-nodes-hash*) t)
			(format str (concatenate 'string
						 "CREATE (" node-name ")-[:"
						 relation-name "]->(" object-node-name ")~%"))))))))))

      
      ;; start code for cypher-from-files (output-file-path text-and-meta-pairs)
      (generateNeo4jCategoryNodes) ;; just once, not for every input file
      (dolist (pair text-and-meta-pairs)
	(cypher-from-files-handle-single-file (car pair) (cadr pair))
	(let ((h (entities_dbpedia:find-entities-in-text (file-to-string (car pair)))))
	  (entities_dbpedia:entity-iterator #'generate-dbpedia-contains-cypher h))))))


(defvar test_files '((#P"~/GITHUB/common-lisp/kgcreator/test_data/test3.txt"
		      #P"~/GITHUB/common-lisp/kgcreator/test_data/test3.meta")
		     (#P"~/GITHUB/common-lisp/kgcreator/test_data/test2.txt"
		      #P"~/GITHUB/common-lisp/kgcreator/test_data/test2.meta")
		     (#P"~/GITHUB/common-lisp/kgcreator/test_data/test1.txt"
		      #P"~/GITHUB/common-lisp/kgcreator/test_data/test1.meta")))

(defun test2a ()
  (cypher-from-files "out.cypher" test_files))

