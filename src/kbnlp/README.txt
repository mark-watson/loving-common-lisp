This is the stub README.txt for the "kbnlp" project.

From my book URI: https://leanpub.com/lovinglisp

Clone this repo into ~/quicklisp/local-projects and then try:


````
(ql:quickload "kbnlp")
(kbnlp:make-text-object "President Bill Clinton ran for president of the USA")
(kbnlp:make-text-object "President Bill Clinton ran for president of the USA. He campaigned on better public health care. Clinton was criticized for military actions in Yugoslavia and also for lying to Congress.")
````

## Making a standalone executable (just one file):

````
$ sbcl
* (ql:quickload "kbnlp")
* (defun test123 () (print (kbnlp:make-text-object "President Bill Clinton ran for president of the USA")))
* (sb-ext:save-lisp-and-die "kbnlptest" :toplevel #'test123 :executable t)

## Function Documentation

### make-text-object (words &key (url "") (title ""))

This is the main entry point for the NLP library. It takes a string of words (or a list of words) and returns a `text` object. The `text` object contains the original text, part-of-speech tags, human names, place names, company names, category tags, and a summary.

### find-names-places (txt-object)

This function finds human and place names in a `text` object. It returns a list of two lists: the first list contains human names, and the second list contains place names.

### summarize (txt-obj)

This function generates a summary of the text in a `text` object. It ranks sentences based on word frequency and returns the most important ones.

### get-word-list-category (words)

This function assigns category tags to a list of words. It returns a list of categories with their scores.