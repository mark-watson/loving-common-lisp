;;;; clml.asd

(asdf:defsystem #:clmltest
  :description "Describe clml here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (:clml
               :clml.utility ; Need clml.utility.data to get data from the net
               :clml.hjs ; Need clml.hjs.read-data to poke around the raw dataset
               :clml.pca
               :clml.clustering)
  :serial t
  :components ((:file "package")
               (:file "clml_data_apis")
               (:file "clml_kmeans_clustering")
               (:file "clml_svm_classifier")
               (:file "clml_test")
               (:file "info")
               ))

(print "***** defsystem clmltest.asd loaded.")
