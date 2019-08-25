;; note; run SBCL using: sbcl --dynamic-space-size 2560

(ql:quickload '(:clml
                :clml.hjs ; utilities
                :clml.svm))

;;(defpackage #:clml-svm-classifier-example1
;;  (:use #:cl #:clml.hjs.read-data))

(in-package #:clmltest)

(defun svm-classifier-test (kernel train test)
  "train and test are lists of lists, with first elements being negative
   samples and the second elements being positive samples"
  (let ((decision-function (clml.svm.mu:svm kernel (cadr train) (car train)))
          (correct-positives 0)
          (wrong-positives 0)
          (correct-negatives 0)
          (wrong-negatives 0))
    ;; type: #<CLOSURE (LAMBDA (CLML.SVM.MU::Z) :IN CLML.SVM.MU::DECISION)>
    (print decision-function)
    (princ "***** NEGATIVE TESTS: calling decision function:")
    (terpri)
    (dolist (neg (car test))  ;; negative test examples
      (let ((prediction (funcall decision-function neg)))
        (print prediction)
        (if prediction (incf wrong-negatives) (incf correct-negatives))))
    (princ "***** POSITIVE TESTS: calling decision function:")
    (terpri)
    (dolist (pos (cadr test)) ;; positive test examples
      (let ((prediction (funcall decision-function pos)))
        (print prediction)
        (if prediction (incf correct-positives) (incf wrong-positives))))
    (format t "Number of correct negatives ~a~%" correct-negatives)
    (format t "Number of wrong negatives ~a~%" wrong-negatives)
    (format t "Number of correct positives ~a~%" correct-positives)
    (format t "Number of wrong positives ~a~%" wrong-positives)))


(defun cancer-data-svm-example-read-data ()

  (defun split-positive-negative-cases (data)
    (let ((negative-cases '())
          (positive-cases '()))
      (dolist (d data)
        ;;(print (list "*  d=" d))
        (if (equal (symbol-name (first (last d))) "benign")
            (setf negative-cases (cons (reverse (cdr (reverse d))) negative-cases))
            (setf positive-cases (cons (reverse (cdr (reverse d))) positive-cases))))
      (list negative-cases positive-cases)))

  (let* ((train1
          (clml.hjs.read-data:read-data-from-file
           "../machine_learning_data/labeled_cancer_training_data.csv"
           :type :csv
           :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                  '(symbol))))
         (train-as-list
          (split-positive-negative-cases
           (coerce
            (map 'list
                 #'(lambda (x) (coerce x 'list))
                 (coerce (clml.hjs.read-data:dataset-points train1) 'list))
            'list)))
         (test1
          (clml.hjs.read-data:read-data-from-file
           "../machine_learning_data/labeled_cancer_test_data.csv"
           :type :csv
           :csv-type-spec (append (make-list 9 :initial-element 'double-float)
                                  '(symbol))))
         (test-as-list
          (split-positive-negative-cases
           (coerce
            (map 'list
                 #'(lambda (x) (coerce x 'list))
                 (coerce (clml.hjs.read-data:dataset-points test1) 'list))
            'list))))

    ;; we will use a gaussian kernel for numeric data.
    ;; note: for text classification, use a clml.svm.mu:+linear-kernel+
    (svm-classifier-test
     (clml.svm.mu:gaussian-kernel 2.0d0)
     train-as-list test-as-list)))

(defun clml-cancer-data-svm-example()
  (cancer-data-svm-example-read-data))
