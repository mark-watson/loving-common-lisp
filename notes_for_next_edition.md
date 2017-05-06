1. suggestion from Edward Geist:

Easier way to load DeepLearning4j code (instead of building a JAR):

(require :abcl-contrib)
(defsystem :dl4j :defsystem-depends-on (:abcl-asdf)
  :components ((:mvn "org.deeplearning4j/deeplearning4j-core" :version "0.8.0")))

2. suggestion from Olof-Joachim Frahm:

Calling Java code from ABCL:

"""
the Lisp code should now probably be using JSS to make it less verbose (dunno what the best link is, perhaps https://abcl-dev.blogspot.de/2012/02/jss-3.html).
The static methods would be called like (#"currentTimeMillis" 'java.lang.System), or in the case of Simple (which also doesn't need the imports):
CL-USER(4): (#"modifyString" 'Simple "test")
"test123"
Otherwise static methods should be called with JSTATIC, e.g.:
CL-USER(6): (jstatic "modifyString" (jclass "Simple") "test")
"test123"
""""
