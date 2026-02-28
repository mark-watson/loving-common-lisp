;; Compyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

(in-package #:conceptnet)

(defun hash-keys-and-values (hash-table)
  (loop for key being the hash-keys of hash-table collect (list key (gethash key hash-table))))

(defun get-surface-text (an-edge)
  (dolist (e an-edge)
    (if (equal (car e) :surface-text)
	(return-from get-surface-text (cdr e))))
  nil)

(defun conceptnet (query)
  (print query)
  (let* ((response
          (uiop:run-program 
           (list
            "curl" 
            (concatenate 'string
                         "https://api.conceptnet.io/c/en/"
                         query))
           :output :string))
	 (results
	  (with-input-from-string
	      (s response)
	    (json:decode-json s))))
    (remove nil (mapcar #'get-surface-text (cdr (assoc :edges results))))))
    

#|
(setf dd (conceptnet:conceptnet "arizona"))
(pprint dd)
(conceptnet:conceptnet "prescott")
|#
