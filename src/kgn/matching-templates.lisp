(ql:quickload :trivia)

;; who is <name>, <name>, etc. -> (entity_lookup EntityTypes.PERSON <name>)
;; synonyms: who, what, tell, show

(defun process-query (query)
  (let ((ret '()))
    ;; who..
    ;; where...
    (let ((entities (entity-uris:
                     (print "test trivia matching")))))))

(trivia:match '(:a (3 4) 5)
  ((list :a (list _ c) _)
   c))

(setf json-as-list
  '((:HEAD (:VARS "s" "p" "o"))
    (:RESULTS
     (:BINDINGS
      ((:S (:TYPE . "uri") (:VALUE . "http://wikiba.se/ontology#Dump"))
       (:P (:TYPE . "uri") (:VALUE . "http://creativecommons.org/ns#license"))
       (:O
        (:TYPE . "uri")
        (:VALUE . "http://creativecommons.org/publicdomain/zero/1.0/")))
      ((:S (:TYPE . "uri") (:VALUE . "http://wikiba.se/ontology#Dump"))
       (:P (:TYPE . "uri") (:VALUE . "http://schema.org/softwareVersion"))
       (:O (:TYPE . "literal") (:VALUE . "1.0.0")))))))

(trivia:match json-as-list
  ((list (list :HEAD (list* :VARS (list* v)))
         (list :RESULTS
               (list* :BINDINGS (list* bindings))))
   (list v bindings)))
