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


