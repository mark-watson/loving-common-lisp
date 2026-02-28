(in-package #:kgn-capi-ui)

;; (use-package "CAPI")

(defun prompt-selection-list (a-list-of-choices) 
  ;; '((:people (("11" "data1")  ("22" "data2"))) (:places (("p1" "data3") ("p2" "data4") ("p3" "data5"))))
  
  (format t "~%~%~%__***__ user-interface.lisp prompt-selection-list___***___ a-list-of-coices:~%~%") (pprint a-list-of-choices) (format t "~%____****_____~%~%")
  (let (ret)
    (dolist (choice a-list-of-choices)
      (setf choice (remove-if #'null choice)) ;; TBD: using new ask sparql lookups, sometimes nil vals in choice list
      (format t "~%______ user-interface.lisp prompt-selection-list_________ choice:~%~%") (pprint choice) (format t "~%_____________~%~%")
      (let* ((topic-type (car choice))
             (choice-list-full (rest choice))
             (choice-list (remove-duplicates
                           (map 'list #'(lambda (z)
                                          (list
                                           z ;; (first z)
                                           (string-shorten
                                            (kgn-common:clean-comment
                                             (kgn-common:clean-comment (cadr z))) ;; (kgn-common:clean-comment (cadar z)))
                                            140 :first-remove-stop-words t)))
                                (apply #'append choice-list-full)) ;; top level list flatten
                           :test #'equal)))
        (format t "~%~% ^^^^^^^^^ choice-list-full:~%~%") (pprint choice-list-full) (terpri)
        (format t "~%~% @@@@@@@@@ choice-list:~%~%") (pprint choice-list) (terpri)
        (let ((dialog-results (alexandria:flatten
                               (capi:prompt-with-list ;; SHOW SELECTION LIST
                                                      (map 'list #'second choice-list) ;;#'second choice-list)
                                                      (symbol-name topic-type)
                                                      :interaction :multiple-selection
                                                      :choice-class 'capi:button-panel
                                                      :pane-args '(:visible-min-width 910
                                                                   :layout-class capi:column-layout))))
              (ret2))
          (dolist (x choice-list)
            (if (find (second x) dialog-results)
                (setf ret2 (cons (car x) ret2))))
          (if (> (length ret2) 0)
              (setf ret (cons (list topic-type (reverse ret2)) ret))))))
    (reverse ret)))

(defun t3 ()
  (prompt-selection-list  '((:people (("11" "data1")  ("22" "data2"))) (:places (("p1" "data3") ("p2" "data4") ("p3" "data5"))))))

;; (get-entity-data-helper "Bill Gates went to Seattle to Microsoft")
;; (prompt-selection-list (get-entity-data-helper "Bill Gates went to Seattle to Microsoft"))
