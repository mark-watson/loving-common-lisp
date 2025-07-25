This is the stub README.txt for the "kbnlp" project.

From my book URI: https://leanpub.com/lovinglisp

Clone this repo into ~/quicklisp/local-projects and then try:


````
 (ql:quickload "fasttag")
 (fasttag:part-of-speech-tagger "President Bush went to China. He wanted a good trade agreement.")

````

## Making a standalone executable (just one file):

````
$ sbcl
* (ql:quickload "kbnlp")
* (defun test123 () (print (kbnlp:make-text-object "President Bill Clinton ran for president of the USA")))
* (sb-ext:save-lisp-and-die "kbnlptest" :toplevel #'test123 :executable t)


