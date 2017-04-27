(add-to-classpath '("uber-lisp-deep-learning.jar"))
(add-to-classpath '("dl.jar"))

(defun train (numInput numHidden numberOfLayers
              labelColumnIndex numSamples
              iterations trainingCsvFilePath)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "com.markwatson.deeplearning.LispInterface"))
         (int-class (jclass "int"))
         (string-class (jclass "java.lang.String"))
         (method (jmethod class "trainNetwork" int-class int-class
                          int-class int-class int-class
                          int-class string-class))
         (result (jcall method param numInput numHidden numberOfLayers
                        labelColumnIndex numSamples
                        iterations trainingCsvFilePath)))
    (format t "in mls, result of calling trainNetwork(...): ~a~%" result)
    result))

;; copied from JSS contrib: jarray-to-list:
(defun jarray-to-list (jarray)
  "Convert the Java array named by JARRARY into a Lisp list."
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :from 0 :below (jarray-length jarray)
        :collecting (jarray-ref jarray i)))

(defun evaluateData (dataCsvFilePath)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "com.markwatson.deeplearning.LispInterface"))
         (string-class (jclass "java.lang.String"))
         (method (jmethod class "evaluateData" string-class))
         (result (jcall method param dataCsvFilePath)))
    (format t "in mls, result of calling trainNetwork(...): ~a~%" result)
    (jarray-to-list result)))

;; (train 9 3 3 9 600 100 "data/training.csv")

;; (evaluateData "data/try_it_out.csv")
