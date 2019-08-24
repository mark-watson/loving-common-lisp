(in-package #:kgcreator)

(let ((*rdf-nodes-hash*))
  
  (defun rdf-from-files (output-file-path text-and-meta-pairs)
    (setf *rdf-nodes-hash* (make-hash-table :test #'equal :size 200))
    (print (list "==> rdf-from-files" output-file-path text-and-meta-pairs ))
    (with-open-file
        (str output-file-path
             :direction :output
             :if-exists :supersede
             :if-does-not-exist :create)

      (defun rdf-from-files-handle-single-file (text-input-file meta-input-file)
        (let* ((text (file-to-string text-input-file))
               (words (myutils:words-from-string text))
               (meta (file-to-string meta-input-file))) ;; TBD parse JSON
          
          (defun generate-original-doc-node-rdf ()
            (let ((node-name (node-name-from-uri meta)))
              (if (null (gethash node-name *rdf-nodes-hash*))
                  (let* ((cats (categorize words))
                         (sum (summarize words cats)))
		    (print (list "$$$$$$  cats:" cats))
                    (setf (gethash node-name *rdf-nodes-hash*) t)
		    (format str (concatenate 'string "<" meta
					     "> <http:knowledgebooks.com/schema/summary> \""
					     sum "\" . ~%"))
                    (dolist (cat cats)
                      (let ((hash-check (concatenate 'string node-name (car cat))))
                        (if (null (gethash hash-check *rdf-nodes-hash*))
                            (let ()
                              (setf (gethash hash-check *rdf-nodes-hash*) t)
			      (format str
				      (concatenate 'string "<" meta
						   "> <http://knowledgebooks.com/schema/topicCategory> "
						   "<http://knowledgebooks.com/schema/"
						   (car cat) "> . ~%"))))))))))
          
          (defun generate-dbpedia-contains-rdf (key value)
            (generate-original-doc-node-rdf)
            (let ((relation-name (concatenate 'string key "DbPediaLink")))
              (dolist (entity-pair value)
                (let* ((node-name (node-name-from-uri meta))
                       (object-node-name (node-name-from-uri (cadr entity-pair)))
                       (hash-check (concatenate 'string node-name object-node-name)))
                  (if (null (gethash hash-check *rdf-nodes-hash*))
                      (let ()
                        (setf (gethash hash-check *rdf-nodes-hash*) t)
			(format str (concatenate 'string "<" meta
					       "> <http://knowledgebooks.com/schema/contains/"
					       key "> " (cadr entity-pair) " .~%"))))))))))

      
      ;; start code for rdf-from-files (output-file-path text-and-meta-pairs)
      (dolist (pair text-and-meta-pairs)
        (rdf-from-files-handle-single-file (car pair) (cadr pair))
        (let ((h (entities_dbpedia:find-entities-in-text (file-to-string (car pair)))))
          (entities_dbpedia:entity-iterator #'generate-dbpedia-contains-rdf h))))))


(defvar test_files '((#P"~/GITHUB/common-lisp/kgcreator/test_data/test3.txt"
                      #P"~/GITHUB/common-lisp/kgcreator/test_data/test3.meta")))
(defvar test_filesZZZ '((#P"~/GITHUB/common-lisp/kgcreator/test_data/test3.txt"
                         #P"~/GITHUB/common-lisp/kgcreator/test_data/test3.meta")
                        (#P"~/GITHUB/common-lisp/kgcreator/test_data/test2.txt"
                         #P"~/GITHUB/common-lisp/kgcreator/test_data/test2.meta")
                        (#P"~/GITHUB/common-lisp/kgcreator/test_data/test1.txt"
                         #P"~/GITHUB/common-lisp/kgcreator/test_data/test1.meta")))

(defun test3a ()
  (rdf-from-files "out.rdf" test_files))


