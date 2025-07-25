;; Copyright Mark Watson 2001-2022. All Rights Reserved.   http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.
;;
;; Common Lisp stemming code in this file was written by Steven M. Haflich based on the work of Martin Porter.


(in-package :entities)

;; data structures

(defstruct entities
  cities
  companies
  countries
  people
  products
  universities)

(print "Starting to load data....")

(defvar *entities* (make-hash-table :test #'equal :size 4000))

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defvar a1 (write-to-string *base-pathname*))
(setf *current-directory* (subseq a1 3 (- (length a1) 14)))

(defun load-l-file (entity-type entity-type-symbol)
  (let (line)
    (with-open-file
        (in (concatenate 'string *current-directory* "linguistic_data/names." entity-type))
      (dotimes (i 50000)
        (setq line (read-line in nil nil))
        (if (null line) (return))
        (setf (gethash line *entities*) (cons entity-type-symbol (gethash line *entities*)))))))


(load-l-file "cities" :cities)
(load-l-file "companies" :companies)
(load-l-file "countries" :countries)
(load-l-file "people" :people)
(load-l-file "products" :products)
(load-l-file "universities" :universities)

(print "....done loading data.")

(defun words-from-string (str)
  (let ((ss (myutils:tokenize-string str)))
    (make-array (list (length ss)) :initial-contents ss)))

;;                    FIND ENTITY NAMES IN TEXT:

(defun text->entities (words)
  (if (typep words 'string) (setf words (words-from-string words)))
  (let* ((eo (make-entities)) ;; create empty 'entities' struct
         (len (length words))
         word)
    (dotimes (i len)
      (setq word (aref words i))
      ;; process 3 word place names:
      (if (< i (- len 2))
          (let* ((words (concatenate 'string word " " (aref words (1+ i)) " " (aref words (+ i 2))))
                 (lookup-entity-type-list (gethash words *entities*)))
            (dolist (lookup lookup-entity-type-list)
                (case lookup
                  (:cities (setf (entities-cities eo) (cons words (entities-cities eo))))
                  (:companies (setf (entities-companies eo) (cons words (entities-companies eo))))
                  (:countries (setf (entities-countries eo) (cons words (entities-countries eo))))
                  (:people (setf (entities-people eo) (cons words (entities-people eo))))
                  (:products (setf (entities-products eo) (cons words (entities-products eo))))
                  (:universities (setf (entities-universities eo) (cons words (entities-universities eo))))))))
      ;; process 2 word place names:
      (if (< i (1- len))
          (let* ((words (concatenate 'string word " " (aref words (1+ i))))
                 (lookup-entity-type-list (gethash words *entities*)))
            (dolist (lookup lookup-entity-type-list)
                (case lookup
                  (:cities (setf (entities-cities eo) (cons words (entities-cities eo))))
                  (:companies (setf (entities-companies eo) (cons words (entities-companies eo))))
                  (:countries (setf (entities-countries eo) (cons words (entities-countries eo))))
                  (:people (setf (entities-people eo) (cons words (entities-people eo))))
                  (:products (setf (entities-products eo) (cons words (entities-products eo))))
                  (:universities (setf (entities-universities eo) (cons words (entities-universities eo))))))))
      ;; 1 word place names:
      (let* ((words word)
             (lookup-entity-type-list (gethash words *entities*)))
        (dolist (lookup lookup-entity-type-list)
            (case lookup
              (:cities (setf (entities-cities eo) (cons words (entities-cities eo))))
              (:companies (setf (entities-companies eo) (cons words (entities-companies eo))))
              (:countries (setf (entities-countries eo) (cons words (entities-countries eo))))
              (:people (setf (entities-people eo) (cons words (entities-people eo))))
              (:products (setf (entities-products eo) (cons words (entities-products eo))))
              (:universities (setf (entities-universities eo) (cons words (entities-universities eo))))))))
    eo))
