(in-package #:wolfram)

(defun wolfram (statement)
  (uiop:run-program (concatenate 'string "wolframscript -code 'Export[\"test44.txt\","
				 statement ",\"ExpressionJSON\"]'"))
  (let* ((ret (uiop:read-file-string "test44.txt")))
    (delete-file "test44.txt")
    (with-input-from-string (s (myutils:replace-all
				(myutils:replace-all ret "\"'" "\"") "'\"" "\""))
      (json:decode-json s))))

(defun cleanup-lists (r)
  (cdr (recursive-remove "Rule" (recursive-remove "List" r))))

(defun recursive-remove (item tree)
  (if (atom tree)
      tree
      (mapcar (lambda (nested-list) (recursive-remove item nested-list))
              (remove item tree :test #'equal))))

#|

(defvar example "TextCases['NYC, Los Angeles, and Chicago are the largest cities in the USA in 2018 according to Pete Wilson.', {'City', 'Country', 'Date', 'Person'} -> {'String', 'Interpretation', 'Probability'}]")
(defvar example-str (myutils:replace-all  example "'" "\""))
(defvar results (wolfram:wolfram example-str))
(pprint results)

(defvar results-cleaned (wolfram:cleanup-lists results))
(pprint results-cleaned)

|#
