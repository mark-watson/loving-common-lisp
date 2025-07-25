# Using The CL Machine-Learning Library

The CL Machine-Learning (CLML) library was originally developed by MSI (NTT DATA Mathematical Systems Inc. in Japan) and is supported by many developers. You should visit the [CLML web page](https://github.com/mmaul/clml) for project documentation and follow the installation directions and read about the project before using the examples in this chapter. However if you just want to quickly try the following CLML examples then you can install CLML using Quicklisp:

~~~~~~~~
mkdir -p ~/quicklisp/local-projects
cd ~/quicklisp/local-projects
git clone https://github.com/mmaul/clml.git
sbcl --dynamic-space-size 2560
> (ql:quickload :clml :verbose t)
~~~~~~~~

The installation will take a while to run but after installation using the libraries via quickload is fast. You can now run the example Quicklisp project **src/clml_examples**:

{lang="lisp",linenos=off}
~~~~~~~~
$ sbcl --dynamic-space-size 2560
* (ql:quickload "clmltest")
* (clmltest:clml-tests-example)
~~~~~~~~

Please be patient the first time you run this because the first time you load the example project, the one time installation of CLML will take a while to run but after installation then the example project loads quickly. CLML installation involves downloading and installing BLAS, LAPACK, and other libraries.

Other resources for CLML are the [tutorials](https://github.com/mmaul/clml.tutorials) and [contributed extensions](https://github.com/mmaul/clml.extras) that include support for plotting (using several libraries) and for fetching data sets.

Although CLML is fairly portable we will be using SBCL and we need to increase the heap space when starting SBCL when we want to use the CLML library:

{linenos=off}
~~~~~~~~
sbcl --dynamic-space-size 5000
~~~~~~~~

You can refer to the documentation at [https://github.com/mmaul/clml](https://github.com/mmaul/clml). This documentation lists the packages with some information for each package but realistically I keep the source code for CLML in an editor or IDE and read source code while writing code that uses CLML. I will show you with short examples how to use the KNN (K nearest neighbors) and SVM (support vector machines) APIs. We will not cover other useful CLML APIs like time series processing, Naive Bayes, PCA (principle component analysis) and general matrix and tensor operations.

Even though the learning curve is a bit steep, CLML provides a lot of functionality for machine learning, dealing with time series data, and general matrix and tensor operations.


## Using the CLML Data Loading and Access APIs

The CLML project uses several data sets and since the few that we will use are small files, they are included in the book's repository in directory *machine_learning_data* under the *src* directory. The first few lines of *labeled_cancer_training_data.csv* are:

{linenos=off}
~~~~~~~~
Cl.thickness,Cell.size,Cell.shape,Marg.adhesion,Epith.c.size,Bare.nuclei,Bl.cromatin,Normal.nucleoli,Mitoses,Class
5,4,4,5,7,10,3,2,1,benign
6,8,8,1,3,4,3,7,1,benign
8,10,10,8,7,10,9,7,1,malignant
2,1,2,1,2,1,3,1,1,benign
~~~~~~~~

The first line in the CSV data files specifies names for each attribute with the name of the last column being "Class" which here takes on values *benign* or *malignant*. Later, the goal will be to create models that are constructed from training data and then make predictions of the "Class" of new input data. We will look at how to build and use machine learning models later but here we concentrate on reading and using input data.

The example file *clml_data_apis.lisp* shows how to open a file and loop over the values for each row:

{lang="lisp",linenos=on}
~~~~~~~~
;; note; run SBCL using: sbcl --dynamic-space-size 2560

(ql:quickload '(:clml
                :clml.hjs)) ; read data sets

(defpackage #:clml-data-test
  (:use #:cl #:clml.hjs.read-data))

(in-package #:clml-data-test)

(defun read-data ()
  (let ((train1
         (clml.hjs.read-data:read-data-from-file
          "./machine_learning_data/labeled_cancer_training_data.csv"
          :type :csv
          :csv-type-spec (append
                           (make-list 9 :initial-element 'double-float)
                           '(symbol)))))
    (loop-over-and-print-data train1)))

(defun loop-over-and-print-data (clml-data-set)
  (print "Loop over and print a CLML data set:")
  (let ((testdata (clml.hjs.read-data:dataset-points clml-data-set)))
    (loop for td across testdata
       do
         (print td))))

(read-data)
~~~~~~~~

The function **read-data** defined in lines 11-19 uses the utility function **clml.hjs.read-data:read-data-from-file** to read a CSV (comma separated value) spreadsheet file from disk. The CSV file is expected to contain  10 columns (set in lines 17-18) with the first nine columns containing floating point values and the last column text data.

The function **loop-over-and-print-data** defined in lines 21-26 reads the CLML data set object, looping over each data sample (i.e., each row in the original spreadsheet file) and printing it.

Here is some output from loading this file:

~~~~~~~~
$ sbcl --dynamic-space-size 2560
This is SBCL 1.3.16, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (load "clml_data_apis.lisp")

"Loop over and print a CLML data set:" 
#(5.0d0 4.0d0 4.0d0 5.0d0 7.0d0 10.0d0 3.0d0 2.0d0 1.0d0 |benign|) 
#(6.0d0 8.0d0 8.0d0 1.0d0 3.0d0 4.0d0 3.0d0 7.0d0 1.0d0 |benign|) 
#(8.0d0 10.0d0 10.0d0 8.0d0 7.0d0 10.0d0 9.0d0 7.0d0 1.0d0 |malignant|) 
#(2.0d0 1.0d0 2.0d0 1.0d0 2.0d0 1.0d0 3.0d0 1.0d0 1.0d0 |benign|) 
~~~~~~~~

In the next section we will use the same cancer data training file, and another test data in the same format to cluster this cancer data into similar sets, one set for non-malignant and one for malignant samples.

## K-Means Clustering of Cancer Data Set

We will now read the same University of Wisconsin cancer data set and cluster the input samples (one sample per row of the spreadsheet file) into similar classes. We will find after training a model that the data is separated into two clusters, representing non-malignant and malignant samples.

The function **cancer-data-cluster-example-read-data** defined in lines 33-47 is very similar to the function **read-data** in the last section except here we read in two data files: one for training and one for testing.

The function **cluster-using-k-nn** defined in lines 13-30 uses the training and test data objects to first train a model and then to test it with test data that was previously used for training. Notice how we call this function in line 47: the first two arguments are the two data set objects, the third is the string "Class" that is the label for the 10th column of the original spreadsheet CSV files, and the last argument is the type of distance measurement used to compare two data samples (i.e., comparing any two rows of the training CSV data file).

{lang="lisp",linenos=on}
~~~~~~~~
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
            (clml.nearest-search.k-nn:k-nn-analyze train
              k
              objective-param-name :all
              :distance manhattan :normalize t)))
      (loop for data across
          (dataset-points
            (clml.nearest-search.k-nn:k-nn-estimate k-nn-estimator test))
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
~~~~~~~~

The following listing shows the output from running the last code example:

~~~~~~~~
Number of self-misjudgement : 13
Correct: #(benign 5.0d0 1.0d0 1.0d0 1.0d0 2.0d0 1.0d0 3.0d0 1.0d0 1.0d0 benign)
Correct: #(benign 3.0d0 1.0d0 1.0d0 1.0d0 2.0d0 2.0d0 3.0d0 1.0d0 1.0d0 benign)
Correct: #(benign 4.0d0 1.0d0 1.0d0 3.0d0 2.0d0 1.0d0 3.0d0 1.0d0 1.0d0 benign)
Correct: #(benign 1.0d0 1.0d0 1.0d0 1.0d0 2.0d0 10.0d0 3.0d0 1.0d0 1.0d0 benign)
Correct: #(benign 2.0d0 1.0d0 1.0d0 1.0d0 2.0d0 1.0d0 1.0d0 1.0d0 5.0d0 benign)
Correct: #(benign 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 1.0d0 3.0d0 1.0d0 1.0d0 benign)
Wrong:   #(benign 5.0d0 3.0d0 3.0d0 3.0d0 2.0d0 3.0d0 4.0d0 4.0d0 1.0d0
           malignant)
Correct: #(malignant 8.0d0 7.0d0 5.0d0 10.0d0 7.0d0 9.0d0 5.0d0 5.0d0 4.0d0
           malignant)
Correct: #(benign 4.0d0 1.0d0 1.0d0 1.0d0 2.0d0 1.0d0 2.0d0 1.0d0 1.0d0 benign)
Correct: #(malignant 10.0d0 7.0d0 7.0d0 6.0d0 4.0d0 10.0d0 4.0d0 1.0d0 2.0d0
           malignant)
 ...
~~~~~~~~


## SVM Classification of Cancer Data Set

We will now reuse the same cancer data set but use a different way to classify data into non-malignant and malignant categories: Support Vector Machines (SVM). SVMs are linear classifiers which means that they work best when data is linearly separable. In the case of the cancer data, there are nine dimensions of values that (hopefully) predict one of the two output classes (or categories). If we think of the first 9 columns of data as defining a 9-dimensional space, then SVM will work well when a 8-dimensional hyperplane separates the samples into the two output classes (categories).

To make this simpler to visualize, if we just had two input columns, that defines a two-dimensional space, and if a straight line can separate most of the examples into the two output categories, then the data is linearly separable so SVM is a good technique to use. The SVM algorithm is effectively determining the parameters defining this one-dimensional line (or in the cancer data case, the 9-dimensional hyperspace).

What if data is not linearly separable? Then use the backpropagation neural network code in the chapter "Backpropagation Neural Networks" or the deep learning code in the chapter "Using Armed Bear Common Lisp With DeepLearning4j" to create a model.

SVM is very efficient so it often makes sense to first try SVM and if trained models are not accurate enough then use neural networks, including deep learning.

The following listing of file *clml_svm_classifier.lisp* shows how to read data, build a model and evaluate the model with different test data. In line 15 we use the function **clml.svm.mu:svm** that requires the type of kernel function to use, the training data, and testing data. Just for reference, we usually use Gaussian kernel functions for processing numeric data and linear kernel functions for handling text in natural language processing applications. Here we use a Gaussian kernel.

The function **cancer-data-svm-example-read-data** defined on line 40 differs from how we read and processed data earlier because we need to separate out the *positive* and *negative* training examples. The data is split in the lexically scoped function in lines 42-52. The last block of code in lines 54-82 is just top-level test code that gets executed when the file *clml_svm_classifier.lisp* is loaded.

{lang="lisp",linenos=on}
~~~~~~~~
;; note; run SBCL using: sbcl --dynamic-space-size 2560

(ql:quickload '(:clml
                :clml.hjs ; utilities
                :clml.svm))

(defpackage #:clml-svm-classifier-example1
  (:use #:cl #:clml.hjs.read-data))

(in-package #:clml-svm-classifier-example1)

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
            (setf negative-cases
                  (cons (reverse (cdr (reverse d))) negative-cases))
            (setf positive-cases
                  (cons (reverse (cdr (reverse d))) positive-cases))))
      (list negative-cases positive-cases)))

  (let* ((train1
          (clml.hjs.read-data:read-data-from-file
           "./machine_learning_data/labeled_cancer_training_data.csv"
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
           "./machine_learning_data/labeled_cancer_test_data.csv"
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

(cancer-data-svm-example-read-data)
~~~~~~~~

The sample code prints the prediction values for the test data which I will not show here. Here are the last four lines of output showing the cumulative statistics for the test data:

~~~~~~~~
Number of correct negatives 219
Number of wrong negatives 4
Number of correct positives 116
Number of wrong positives 6
~~~~~~~~



## CLML Wrap Up

The CLML machine learning library is under fairly active development and I showed you enough to get started: understanding the data APIs and examples for KNN clustering and SVM classification.

A good alternative to CLML is [MGL](https://github.com/melisgl/mgl) that supports backpropagation neural networks, boltzmann machines, and gaussian processes.

In the next two chapters we continue with the topic of machine learning with backpropagation andf Hopfield neural networks.
