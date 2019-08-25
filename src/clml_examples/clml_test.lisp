;; note; run SBCL using: sbcl --dynamic-space-size 2560

(ql:quickload '(:clml
                :clml.utility ; Need clml.utility.data to get data from the net
                :clml.hjs ; Need clml.hjs.read-data to poke around the raw dataset
                :clml.pca
                :clml.clustering
                ;;:iolib
                ))

(in-package #:clmltest)

(defun similar-word-test (word1 word2)
  (let ((word-similarity (clml.text.utilities:calculate-levenshtein-similarity word1 word2)))
    (format t "similarity of word ~s and word ~s is ~a ~%" word1 word2 word-similarity)))

(similar-word-test "density" "dancing")
(similar-word-test "fun" "funny")
(similar-word-test "xyz" "abc")

;; code from CLML unit test: test-classifier.lisp:
(defun classify-k-nn (test train objective-param-name  manhattan)
  (let (original-data-column-length)
    (setq original-data-column-length
          (length (aref (clml.hjs.read-data:dataset-points train) 0)))
    (let* ((k 5)
           (k-nn-estimator
            (clml.nearest-search.k-nn:k-nn-analyze train k objective-param-name :all :distance manhattan :normalize t)))
      (loop for data across (dataset-points (clml.nearest-search.k-nn:k-nn-estimate k-nn-estimator test))
         with true = 0
         with false = 0
         if (equal (aref data 0) (aref data original-data-column-length))
         do (incf true)
         else do (incf false)
         finally (return (values true false (/ true (+ true false))))))
    ))

;; code from CLML unit test:
(defun cancer-data-classifier-example ()
  (let ((train1
         (clml.hjs.read-data:read-data-from-file
          "../machine_learning_data/labeled_cancer_training_data.csv"
          :type :csv
          :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                 '(symbol))))
        (test1
         (clml.hjs.read-data:read-data-from-file
          "../machine_learning_data/labeled_cancer_test_data.csv"
          :type :csv
          :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                 '(symbol)))))
    (print test1)
    (print (classify-k-nn test1 train1 "Class" :double-manhattan))))

;;(cancer-data-classifier-example)

;; code from CLML unit test: test-classifier.lisp:
(defun classify-k-nn2 (test train objective-param-name  manhattan)
  (print "My tests 2: train data:")
  (print train)
  (print "My tests: test data:")
  (print test)
  (let (original-data-column-length)
    (setq original-data-column-length
          (length (aref (clml.hjs.read-data:dataset-points train) 0)))
    (let* ((k 5)
           (k-nn-estimator
            (clml.nearest-search.k-nn:k-nn-analyze train k objective-param-name :all :distance manhattan :normalize t)))
      (print "results for test data:")
      (print (dataset-points (clml.nearest-search.k-nn:k-nn-estimate k-nn-estimator test))))))

;;(cancer-data-classifier-example2)

;; code from CLML unit test:
(defun cancer-data-classifier-example3 ()
  (let ((train1
         (clml.hjs.read-data:read-data-from-file
          "../machine_learning_data/labeled_cancer_training_data.csv"
          :type :csv
          :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                 '(symbol)))))
    (read-and-print-data train1)))


;; code from CLML unit test: test-classifier.lisp:
(defun read-and-print-data (clml-data-set)
  (print "My tests for reading nd printing a CLML data set:")
  (let ((testdata (clml.hjs.read-data:dataset-points clml-data-set)))
    (loop for td across testdata
       do
         (print td))))

(defun clml-tests-example()
  (cancer-data-classifier-example3))
