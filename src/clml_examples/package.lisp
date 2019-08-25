;;;; package.lisp

(defpackage #:clmltest
  (:use #:cl #:clml.hjs.read-data)
  (:export similar-word-test classify-k-nn clml-cancer-data-svm-example
    clml-read-data-example clml-kmeans-example clml-tests-example))

(print "!!!!! defpackage clmltest package.lisp loaded.")

