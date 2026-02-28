# NO LONGRER IN BOOK

# Using Armed Bear Common Lisp With DeepLearning4j

This chapter both serves as a recipe for calling the Java deep learning library and also as a general example of "wrapping" other complex Java libraries that you may want to call from Common Lisp code that you write.

[DeepLearning4j](https://deeplearning4j.org) is a state of the art deep learning library that can handle small to medium data sets on your laptop and very large data sets using a cluster of servers controlled with [Apache Spark](https://spark.apache.org). Here we just use DeepLearning4j with a small data set and the following examples will run on your laptop if you hav Java 8 installed. Armed Bear Common Lisp (ABCL) is implemented in Java and a compiled version is included in the example directory *src/deep_learning_with_abcl* so the following examples are self contained except for the Java 8 requirement.

## Tutorial on passing data between Common Lisp and Java

The following listing shows the Java code in the source file *Simple.java* in the directory *src/deep_learning_with_abcl*. he first two functions **modifyString** and **addTwoNumbers** return specific types, in this case a string and an integer. The thrid function **modifyArray** takes advantage of ABCL's flexible foreign function interface to Java. We can pass different supported Lisp types to this function and they will be converted automatically to Java types and in returning to Lisp are either converted to simple Lisp types or wrapped as ABCL specific types. We will see more examples of converting data types later.


{lang="java",linenos=on}
~~~~~~~~
import org.armedbear.lisp.*;

public class Simple {
  
  static public String modifyString(String s) {
      return s + "123";
  }
  static public int addTwoNumbers(int a, int b) {
      return a + b;
  }
  static public Object modifyArray(Object values) {
      return values;
  }
}
~~~~~~~~

The following listing of file *simple.lisp* shows how to call the three Java metods defined in the last listing. In lines 8, 17, and 26 we see the use of **jmethod** to wrap a Java class method as a function usable in ABCL. The value in variable **class** (defined in lines 6, 15, and 24) references the name of the class in the Java code, in this case the class was **Simple**. To call into Java code we use **jcall** (lines 9, 18, and 27). The first two calls to **jcall** are simple because we are passing native Java types (**int** and **java.lang.String**). The third example on line 27 is more complicated because we need to convert a Common Lisp list to a Java array.

{lang="lisp",linenos=on}
~~~~~~~~
(add-to-classpath '("Simple.jar"))

(defun add2 (i1 i2)
  (print (jclass "Simple"))
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "Simple"))
         (intclass (jclass "int"))
         (method (jmethod class "addTwoNumbers" intclass intclass))
         (result (jcall method param i1 i2)))
    (format t "in add2, result of calling addTwoNumbers(2, 4): ~a~%" result)
    result))

(defun modify-string (s)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "Simple"))
         (string-class (jclass "java.lang.String"))
         (method (jmethod class "modifyString" string-class))
         (result (jcall method param s)))
    (format t "in add2, result of calling modify-string(...)): ~a~%" result)
    result))

(defun modify-list (ls)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "Simple"))
         (string-class (jclass "java.lang.Object"))
         (method (jmethod class "modifyArray" string-class))
         (result (jcall method param (jarray-from-list ls))))
    (format t "in mls, result of calling modifyArray(...): ~a~%" result)
    result))
~~~~~~~~


Instead of running the following commands manually, you can use the *Makefile* and run "make simple" to compile Simple.java and start a ABCL repl. I show the commands individually here for reference:

~~~~~~~~
javac Simple.java -cp abcl.jar
jar cvf simple.jar Simple.class
java -jar abcl.jar -cp abcl.jar;simple.jar;. Simple
Armed Bear Common Lisp 1.4.0
CL-USER(1): (load "simple.lisp")
T
CL-USER(2): (add2 1 15)

#<java.lang.Class class Simple {24248033}> in add2, result of calling addTwoNumbers(2, 4): 16
16
CL-USER(3): (modify-string "dog")
in add2, result of calling modify-string(...)): dog123
"dog123"
[1] CL-USER(4): (modify-list '(1 2 3))
in mls, result of calling modifyArray(...): #(1 2 3)
#(1 2 3)
[1] CL-USER(5): (modify-list '("cat" "dog"))
in mls, result of calling modifyArray(...): #(cat dog)
#("cat" "dog")
[1] CL-USER(6): 
~~~~~~~~

Note that Java arrays must have elements of the same type so we could not make a call like:

{lang="lisp",linenos=off}
~~~~~~~~
(modify-list '(1 2 3.14 "cat" "dog"))
~~~~~~~~

because an error would be thrown that would look like:

{linenos=off}
~~~~~~~~
#<THREAD "interpreter" {10F00ACA}>: Debugger invoked on condition of type JAVA-EXCEPTION
  Java exception 'java.lang.IllegalArgumentException: array element type mismatch'.
~~~~~~~~


## Using DeepLearning4j Java Code

This example uses both the DeepLearning4j library and also a slightly modified version of an example program in my book [Power Java](https://leanpub.com/powerjava). You can read the book for free online and you might want to read the chapter "Deep Learning Using Deeplearning4j" for background. All required Java code is included in the repository for this book but to run this example you will need to have both Java 8 and maven installed.

The sample data we will use is the University of Wisconsin cancer data.

### Building an uber-jar file with all dependencies

We need a JAR file that contains both the required DeepLearning4j code and also the Java wrapper code that provides an easier interface to call using the Java foreign function interface in ABCL.

In the git repository top level directory, change directory to:

{linenos=off}
~~~~~~~~
cd deep_learning_with_abcl/java_code_for_jar_library/
~~~~~~~~

You need Java 8 and maven installed to build an uber jar (that is a jar with the Java code and all required jar files, including DeepLearning4j dependencies used in the Java code for this project):

{linenos=off}
~~~~~~~~
mvn package
cp target/uber-lisp-deep-learning.jar ..
mvn clean
cd ..
~~~~~~~~

You should now have a new jar file *uber-lisp-deep-learning.jar* in the *deep_learning_with_abcl* directory. If you are curious about the contents of this JAR file then try this:

{linenos=off}
~~~~~~~~
jar tvf uber-lisp-deep-learning.jar
~~~~~~~~

You are seeing compiled code for the wrapper from my Power Java book (modified slighty here to make it easier to call from Lisp), DeepLearning4j, and all other required dependencies. If you want to wrap other Java libraries for use with ABCL then hopefully the code and maven *pom.xml* configuration file in the subdirectory *java_code_for_jar_library* will serve as a useful example.


### Running the Example Lisp Client Code

There is a *Makefile* that builds the local Java client code and starts up ABCL Lisp (following listing has been edited to avoid line wraps so als look at the file *Makefile*):

{linenos=off}
~~~~~~~~
dl: compile_java_dl run_dl

compile_java_dl:
	javac LispInterfaceDataFetcher.java -cp abcl.jar:uber-lisp-deep-learning.jar:.
	javac LispInterfaceDataSetIterator.java -cp \
    abcl.jar:uber-lisp-deep-learning.jar:.
	javac DeepBeliefNetworkLispInterfaceData.java -cp \
    abcl.jar:uber-lisp-deep-learning.jar:.
	javac LispInterface.java -cp abcl.jar:uber-lisp-deep-learning.jar:.
	jar cvf dl.jar DeepBeliefNetworkLispInterfaceData.class \
    LispInterface.class LispInterfaceDataFetcher.class \
    LispInterfaceDataSetIterator.class

run_dl:
	java -jar abcl.jar -cp \
    abcl.jar:abcl-contrib-1.4.0.jar:dl.jar:uber-lisp-deep-learning.jar:. \
    LispInterface

simple: compile_java_simple run_simple

compile_java_simple:
	javac Simple.java -cp abcl.jar
	jar cvf simple.jar Simple.class
	#rm -f Simple.class

run_simple:
	java -jar abcl.jar -cp abcl.jar:simple.jar;. Simple
~~~~~~~~


The following listing of file *deep-learning.lisp* shows the Common Lisp wrapper code for this example. The function **train** (lines 4-18) has arguments defining the size of the neural network (number of neurons in the input layer, in all the hidden layers, and the number of neurons in the output layer. These values should be compatible with the input training comma separated values (CSV) file (i.e., the value for **numInput** should be the number of columns in the CSV file minus one and the value of **labelColumnIndex** should be the column index for the output label.)

ABCL has a contributed library JSS and I copied the utility function **jarray-to-list** to convert Java arrays to Common Lisp lists. The function **evaluateData** (lines 27-34) can be used to try the trained model on new data sets. Using files to pass data is convenient for development and testing and is okay for deployed systems since the overhead of writing and reading files is negligible compared to computation costs of training and using deep learning models.

{lang="lisp",linenos=on}
~~~~~~~~
(add-to-classpath '("uber-lisp-deep-learning.jar"))
(add-to-classpath '("dl.jar"))

(defun train (numInput numHidden numberOfLayers
              labelColumnIndex numSamples
              iterations trainingCsvFilePath)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "LispInterface"))
         (int-class (jclass "int"))
         (string-class (jclass "java.lang.String"))
         (method (jmethod class "trainNetwork" int-class int-class
                          int-class int-class int-class
                          int-class string-class))
         (result (jcall method param numInput numHidden numberOfLayers
                        labelColumnIndex numSamples
                        iterations trainingCsvFilePath)))
    (format t "in mls, result of calling trainNetwork(...): ~a~%" result)
    result))

;; copied from JSS contrib: jarray-to-list:
(defun jarray-to-list (jarray)
  "Convert the Java array named by JARRARY into a Lisp list."
  (declare (optimize (speed 3) (safety 0)))
  (loop :for i :from 0 :below (jarray-length jarray)
        :collecting (jarray-ref jarray i)))

(defun evaluateData (dataCsvFilePath)
  (let* ((param nil) ;; ? not sure about this
         (class (jclass "LispInterface"))
         (string-class (jclass "java.lang.String"))
         (method (jmethod class "evaluateData" string-class))
         (result (jcall method param dataCsvFilePath)))
    (format t "in mls, result of calling trainNetwork(...): ~a~%" result)
    (jarray-to-list result)))

;; (train 9 3 3 9 48 100 "data/training.csv")

;; (evaluateData "data/try_it_out.csv")
~~~~~~~~

You can run the code with the sample data using (a lot of printout is not shown here):

{linenos=off}
~~~~~~~~
$ make
Armed Bear Common Lisp 1.4.0
Java 1.8.0_111 Oracle Corporation
Java HotSpot(TM) 64-Bit Server VM
Low-level initialization completed in 0.41 seconds.
Startup completed in 1.896 seconds.
Type ":help" for a list of available commands.
CL-USER(1): (load "deep-learning.lisp")
T
CL-USER(2):  (train 9 3 3 9 600 100 "data/training.csv")
==========================Scores========================================
 Accuracy:  0.7667
 Precision: 0.7667
 Recall:    1
 F1 Score:  0.8679245283018869
===========================================================================
in mls, result of calling trainNetwork(...): done training
"done training"
"done training"
CL-USER(3): (evaluateData "data/try_it_out.csv")
(0 1 0 0 0 0 1 0 0 0 0 0 0 1 1 1 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 1 0 0 0 0 1 0 1 0 1 0 1 0)
~~~~~~~~

In the last output, zero values indicate non-malignant while one values indicate a prediction of malignant for each test data sample.

The DeepLearning4J library implements several types of networks and here we just looked at a simple example that also serves as an example for calling Jaca libraries. While it is often more practical to use languages like Java or Python for deep learning because of available libraries written in these languages, I think a case can be made for hybrid Common Lisp and Java systems, taking advantage of Common Lisp for general programming, knowledge management, symbolic processing, etc., while using neural models for recognizing images, classifying text, linguistic processing, and other use cases that deep learning excels in.
