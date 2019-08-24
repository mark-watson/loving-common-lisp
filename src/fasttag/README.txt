This is the stub README.txt for the "kbnlp" project.

try:

````
        (ql:quickload "fasttag")
````

## Making a standalone executable (just one file):

````
$ sbcl
* (ql:quickload "kbnlp")
* (defun test123 () (print (kbnlp:make-text-object "President Bill Clinton ran for president of the USA")))
* (sb-ext:save-lisp-and-die "kbnlptest" :toplevel #'test123 :executable t)


