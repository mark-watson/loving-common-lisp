;; Compyright 2021-2022 Mark Watson All Rights Reserved.
;; License: Apache 2

(in-package #:kgn-text-ui)

(defun pprint-results (results)
  (dolist (result (car results))
    (terpri)
    (format t  "~A:" (first result))
    (format t " ~A~%" (second result))))


(defun multiple-selections (sel-list)
  (if (not (null sel-list))
      (let ()
        (pprint sel-list)
        (format t "~%- - - - Enter zero or more indices for your desired selections:~%~%")
        (let ((count 0))
          (dolist (sel sel-list)
            (format t "~A  -   ~S ~%~%" count (cadr (assoc :comment (car sel))))
            (setf count (1+ count))))
        (let* ((line (read-line))
               (indices
                (if (> (length line) 0)
                    (mapcar
                     #'parse-integer
                     (myutils:tokenize-string line)))))
          (print indices)
    ;(dolist (index indices)
    ;  (setf ret (cons (nth index str-list)
          indices))))

;; (kgn-text-ui::multiple-selections '("Option 1" "Option 2" "And yet another option 3"))


(defun prompt-selection-list (a-list-of-choices) 
  ;; '((:people (("11" "data1")  ("22" "data2"))) (:places (("p1" "data3") ("p2" "data4") ("p3" "data5"))))
  (let (ret)
    (dolist (choice a-list-of-choices)
      (setf choice (remove-if #'null choice))
      (let* ((topic-type (car choice))
             (choice-list-full (rest choice))
             (choice-list
              (remove-duplicates
               (map 'list #'(lambda (z)
                              (list
                               z ;; (first z)
                               (string-shorten
                                (kgn-common:clean-comment
                                 (kgn-common:clean-comment (cadr z))) ;; (kgn-common:clean-comment (cadar z)))
                                140 :first-remove-stop-words t)))
                    (apply #'append choice-list-full)) ;; top level list flatten
               :test #'equal)))
        (let (ret2
              (dialog-results (multiple-selections choice-list))) ;; (mapcar #'(lambda (x) (cadr (assoc :comment x))) choice-list))))
          (dolist (index dialog-results)
            (setf ret2 (cons (car (nth index choice-list)) ret2)))
          (if (> (length ret2) 0)
              (setf ret (cons (list topic-type (reverse ret2)) ret))))))
    (reverse ret)))

;; (kgn-text-ui::prompt-selection-list  '((:people (("11" "data1")  ("22" "data2"))) (:places (("p1" "data3") ("p2" "data4") ("p3" "data5")))))
;; (kgn-text-ui::prompt-selection-list (kgn-common:get-entity-data-helper "Bill Gates went to Seattle to Microsoft"))

(defun colorize-sparql (str &key (stream t))
  " this could be used to colorize text (as it is in kgn-capi-ui example)"
  ;;(declare (ignore message-stream))
  (declare (ignore stream))
  (format t "~%~S~%" str))

(defun get-string-from-user (text-prompt)
  (format t "~%~S:~%" text-prompt)
  (read-line))


;; Main funtion

(defun kgn-text-ui ()
  (let (prompt
        (message-stream t)
        (results-stream t))
    (loop
       while
        (> (length (setf prompt (get-string-from-user "Enter entity names (people, places, companies, etc."))) 0)
         do
         (let* ((entity-data (get-entity-data-helper prompt :message-stream t)))
           (let ((user-selections (prompt-selection-list entity-data)))
             (dolist (ev user-selections)
               (if (> (length (cadr ev)) 0)
                   (let ()
                     (terpri results-stream)
                     (format results-stream "- - - ENTITY TYPE: ~A - - -" (car ev))
                     ;;(terpri results-stream)
                     (dolist (uri (cadr ev))
                       (setf uri (cadr (assoc :s uri)))
                       (format t "~%~% -- kgn-text-ui: uri = ~A~%~%" uri)
                       (case (car ev)
                         (:people
                          (pprint-results 
                           (kgn-common:dbpedia-get-person-detail  uri :message-stream message-stream :colorize-sparql-function #'colorize-sparql)))
                         (:companies
                          (pprint-results 
                           (kgn-common:dbpedia-get-company-detail uri :message-stream message-stream :colorize-sparql-function #'colorize-sparql)))
                         (:countries
                          (pprint-results
                           (kgn-common:dbpedia-get-country-detail uri :message-stream message-stream :colorize-sparql-function #'colorize-sparql)))
                         (:cities
                          (pprint-results
                           (kgn-common:dbpedia-get-city-detail    uri :message-stream message-stream :colorize-sparql-function #'colorize-sparql)))
                         (:products
                          (pprint-results
                           (kgn-common:dbpedia-get-product-detail uri :message-stream message-stream :colorize-sparql-function #'colorize-sparql))))))))
             (entity-results->relationship-links user-selections :message-stream t)))))) ;; this prints relationships to message stream

;; (kgn-text-ui:kgn-text-ui)