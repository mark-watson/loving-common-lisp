;;;; spacy.lisp

(in-package #:spacy)

;;(ql:quickload :py4cl)
(py4cl:python-exec "from spacystub.parse import parse")
(py4cl:import-function "parse")

(defun nlp (text)
  (parse text))
