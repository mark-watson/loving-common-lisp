;; Copyright Mark Watson 2001-2022. All Rights Reserved.   http://markwatson.com
;; License: AGPL version 3 (http://www.gnu.org/licenses/agpl-3.0.txt)
;; Alternative license: you may also use this software under the Apache 2 License.
;; This copyright notice should not be removed from this file and in files derived from this file.
;;
;; entities.lisp — Named-entity recognition for Common Lisp
;;
;; This file provides a simple dictionary-based named-entity recognition (NER)
;; system. It loads lists of known entity names (cities, companies, countries,
;; people, products, universities) from data files into a hash table, then
;; provides a function `text->entities` that scans input text for matches against
;; those known names. Entity lookups consider 1-word, 2-word, and 3-word phrases.

(in-package :entities)

;; ============================================================================
;; Data Structures
;; ============================================================================
;; The `entities` struct holds the results of entity recognition. Each slot
;; contains a list of matched entity names for that category.

(defstruct entities
  cities       ;; list of matched city names
  companies    ;; list of matched company names
  countries    ;; list of matched country names
  people       ;; list of matched person names
  products     ;; list of matched product names
  universities) ;; list of matched university names

(print "Starting to load data....")

;; ============================================================================
;; Global State
;; ============================================================================
;; *entities* — hash table mapping entity name strings to lists of entity-type
;; keywords (e.g., (:cities :companies)). Uses #'equal for string-key
;; comparison. Multiple keywords per key allow a name to belong to more than
;; one category.

(defvar *entities* (make-hash-table :test #'equal :size 4000))

;; *base-pathname* — resolved at load/compile time to the directory containing
;; this file. Used to locate the linguistic_data/ data files relative to the
;; source.

(defvar *base-pathname* #.(or *compile-file-truename* *load-truename*))
(defvar *current-directory* (uiop:pathname-directory-pathname *base-pathname*))

;; ============================================================================
;; Data Loading
;; ============================================================================
;; load-l-file reads a data file line by line and populates the *entities*
;; hash table. Each line is an entity name; the value stored is a cons pair
;; prepending the entity-type keyword onto any existing list for that key.
;; This means a single name can appear in multiple categories (e.g., "Apple"
;; could be both :companies and :products).

(defun load-l-file (entity-type entity-type-symbol)
  (let (line)
    (with-open-file
        (in (uiop:merge-pathnames* (concatenate 'string "linguistic_data/names." entity-type)
                                   *current-directory*))
      (dotimes (i 50000) ;; read up to 50000 lines per file
        (setq line (read-line in nil nil))
        (if (null line) (return)) ;; stop at EOF
        ;; Prepend the type keyword to the existing entry list for this name.
        ;; Using cons means the list accumulates in reverse order, which is
        ;; fine since order within a category doesn't matter.
        (setf (gethash line *entities*) (cons entity-type-symbol (gethash line *entities*)))))))


;; Load all six entity data files into the hash table
(load-l-file "cities" :cities)
(load-l-file "companies" :companies)
(load-l-file "countries" :countries)
(load-l-file "people" :people)
(load-l-file "products" :products)
(load-l-file "universities" :universities)

(print "....done loading data.")

;; ============================================================================
;; Entity Recognition
;; ============================================================================
;; text->entities — the main entry point for named-entity recognition.
;;
;; Accepts either a string (which is tokenized into words) or a vector of word
;; strings. Returns an `entities` struct populated with any matching names
;; found in the input.
;;
;; The algorithm scans each word position and checks three window sizes:
;;   1. 3-word phrase (word i, i+1, i+2) — e.g., "New York City"
;;   2. 2-word phrase (word i, i+1)     — e.g., "San Francisco"
;;   3. 1-word phrase  (word i)         — e.g., "Google"
;;
;; Longer phrases are checked first so that multi-word entities are matched
;; before their individual words could match as single-word entities. However,
;; note that shorter matches are NOT suppressed when a longer match exists —
;; overlapping matches at different window sizes can coexist in the result.

(defun text->entities (words)
  ;; If a raw string was passed, tokenize it into a word vector first.
  (if (typep words 'string) (setf words (words-from-string words)))
  (let* ((eo (make-entities)) ;; create empty 'entities' struct to collect results
         (len (length words)))
    (flet ((add-matches (phrase)
             ;; Record phrase under every entity type it is known to belong to.
             (dolist (lookup (gethash phrase *entities*))
               (case lookup
                 (:cities (push phrase (entities-cities eo)))
                 (:companies (push phrase (entities-companies eo)))
                 (:countries (push phrase (entities-countries eo)))
                 (:people (push phrase (entities-people eo)))
                 (:products (push phrase (entities-products eo)))
                 (:universities (push phrase (entities-universities eo)))))))
      (dotimes (i len)
        (let ((word (aref words i)))
          ;; --- 3-word entity check (only if at least 3 words remain) ---
          (if (< i (- len 2))
              (add-matches (concatenate 'string word " " (aref words (1+ i)) " " (aref words (+ i 2)))))
          ;; --- 2-word entity check (only if at least 2 words remain) ---
          (if (< i (1- len))
              (add-matches (concatenate 'string word " " (aref words (1+ i)))))
          ;; --- 1-word entity check ---
          (add-matches word))))
    eo))

(defun make-entities-object (text)
  "Analyze text and return an entities struct with categorized entity names.
This is an alias for text->entities as documented in the library's README."
  (text->entities text))
