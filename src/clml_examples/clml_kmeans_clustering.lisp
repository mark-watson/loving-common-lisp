;; note; run SBCL using: sbcl --dynamic-space-size 2560

(ql:quickload '(:clml
                :clml.hjs ; utilities
                :clml.clustering))

(defpackage #:clml-knn-cluster-example1
  (:use #:cl #:clml.hjs.read-data))

(in-package #:clml-knn-cluster-example1)

;; folowing is derived from test code in CLML:
(defun cluster-using-k-nn (test train objective-param-name  manhattan)
  (let (original-data-column-length)
    (setq original-data-column-length
          (length (aref (clml.hjs.read-data:dataset-points train) 0)))
    (let* ((k 5)
           (k-nn-estimator
            (clml.nearest-search.k-nn:k-nn-analyze train k objective-param-name :all :distance manhattan :normalize t)))
      (loop for data across (dataset-points (clml.nearest-search.k-nn:k-nn-estimate k-nn-estimator test))
         if (equal (aref data 0) (aref data original-data-column-length))
         do
           (format t "Correct: ~a~%" data)
         else do
           (format t "Wrong:   ~a~%" data)))))

;; folowing is derived from test code in CLML:
(defun cancer-data-cluster-example-read-data ()
  (let ((train1
         (clml.hjs.read-data:read-data-from-file
          "./machine_learning_data/labeled_cancer_training_data.csv"
          :type :csv
          :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                 '(symbol))))
        (test1
         (clml.hjs.read-data:read-data-from-file
          "./machine_learning_data/labeled_cancer_test_data.csv"
          :type :csv
          :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                 '(symbol)))))
    ;;(print test1)
    (print (cluster-using-k-nn test1 train1 "Class" :double-manhattan))))

(cancer-data-cluster-example-read-data)
