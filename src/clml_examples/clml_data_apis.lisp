;; note; run SBCL using: sbcl --dynamic-space-size 2560

(ql:quickload '(:clml
                :clml.hjs)) ; read data sets

(in-package #:clmltest)

(defun read-data ()
  (let ((train1
         (clml.hjs.read-data:read-data-from-file
          "../machine_learning_data/labeled_cancer_training_data.csv"
          :type :csv
          :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                 '(symbol)))))
    (loop-over-and-print-data train1)))

(defun loop-over-and-print-data (clml-data-set)
  (print "Loop over and print a CLML data set:")
  (let ((testdata (clml.hjs.read-data:dataset-points clml-data-set)))
    (loop for td across testdata
       do
         (print td))))

(defun clml-read-data-example()
  (read-data))
