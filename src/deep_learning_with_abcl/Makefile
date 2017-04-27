run_deeplearning:  # assumed uber-jar built as described in book
	java -jar abcl.jar -cp abcl.jar:abcl-contrib-1.4.0.jar:dl.jar:uber-lisp-deep-learning.jar:. LispInterface

simple: compile_java_simple run_simple

compile_java_simple:
	javac Simple.java -cp abcl.jar
	jar cvf simple.jar Simple.class
	#rm -f Simple.class

run_simple:
	java -jar abcl.jar -cp abcl.jar:simple.jar;. Simple
