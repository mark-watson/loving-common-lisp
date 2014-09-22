;; utility function from the cl-neo4j-tests package:
(defun get-id-from-data (data)
  (cl-neo4j-wrapper::extract-id-from-link
   (cdr (assoc :self data))))

(let ((node-id (get-id-from-data (cl-neo4j:create-node))))
  (print node-id)
  (let ((same-node (cl-neo4j:get-node :node-id node-id)))
    (print same-node)
    (cl-neo4j:set-node-properties
     :node-id node-id
     :properties '((:my-dog . "Rover")))
    (print "Print all properties on node:")
    (print (cl-neo4j:get-node-properties :node-id node-id))
    ;; create a second test node:
    (let* ((second-node-id (get-id-from-data (cl-neo4j:create-node)))
           (test-relationship-id
            (get-id-from-data
             (cl-neo4j:create-relationship :node-id node-id
                                           :to-node-id second-node-id
                                           :relationship-type "node-with-no-dog"))))
      (print "A relationship between two nodes:")
      (print (cl-neo4j:get-relationship :relationship-id test-relationship-id))
      ;; remove test relationship from graph database:
      (cl-neo4j:delete-relationship :relationship-id test-relationship-id)
      ;; remove second test node:
      (cl-neo4j:delete-node :node-id second-node-id))
      
    ;; delete the first test  node:
    (cl-neo4j:delete-node :node-id node-id)))


