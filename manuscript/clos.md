# Common Lisp Object System - CLOS {#clos_chapter}

CLOS was the first ANSI standardized object oriented programming facility. While I do not use classes and objects as often in my Common Lisp programs as I do when using Java and Smalltalk, it is difficult to imagine a Common Lisp program of any size that did not define and use at least a few CLOS classes. 

The example program for this chapter in the file **src/loving_snippets/HTMLstream.lisp**. I used this CLOS class about ten years ago in a demo for my commercial natural language processing product to automatically generate demo web pages.

We are going to start our discussion of CLOS somewhat backwards by first looking at a short test function that uses the **HTMLstream** class. Once we see how to use this example CLOS class, we will introduce a small subset of CLOS by discussing in some detail the implementation of the **HTMLstream** class and finally, at the end of the chapter, see a few more CLOS programming techniques. This book only provides a brief introduction to CLOS; the interested reader is encouraged to do a web search for “CLOS tutorial”.

The macros and functions defined to implement CLOS are a standard part of Common Lisp. Common Lisp supports generic functions, that is, different functions with the same name that are distinguished by different argument types.

## Example of Using a CLOS Class

The file **src/loving_snippets/HTMLstream.lisp** contains a short test program at the end of the file:

{lang="lisp"}
~~~~~~~~
(defun test (&aux x)
    (setq x (make-instance 'HTMLstream))
    (set-header x "test page")
    (add-element x "test text - this could be any element")
    (add-table
         x
         '(("<b>Key phrase</b>" "<b>Ranking value</b>")
           ("this is a test" 3.3)))
    (get-html-string x))
~~~~~~~~

The generic function **make-instance** takes the following arguments:

~~~~~~~~
    make-instance class-name &rest initial-arguments &key ...
~~~~~~~~

There are four generic functions used in the function test:

- set-header - required to initialize class and also defines the page title
- add-element - used to insert a string that defines any type of HTML element
- add-table - takes a list of lists and uses the list data to construct an HTML table
- get-html-string - closes the stream and returns all generated HTML data as a string

The first thing to notice in the function test is that the first argument for calling each of these generic functions is an instance of the class **HTMLstream**. You are free to also define a function, for example, add-element that does not take an instance of the class **HTMLstream** as the first function argument and calls to **add-element** will be routed correctly to the correct function definition.

We will see that the macro **defmethod** acts similarly to **defun** except that it also allows us to define many methods (i.e., functions for a class) with the same function name that are differentiated by different argument types and possibly different numbers of arguments.

## Implementation of the HTMLstream Class

The class **HTMLstream** is very simple and will serve as a reasonable introduction to CLOS programming. Later we will see more complicated class examples that use multiple inheritance. Still, this is a good example because the code is simple and the author uses this class frequently (some proof that it is useful!). The code fragments listed in this section are all contained in the file **src/loving_snippets/HTMLstream.lisp**. We start defining a new class using the macro **defclass** that takes the following arguments:   

{lang="lisp"}
~~~~~~~~
    defclass class-name list-of-super-classes
             list-of-slot-specifications class-specifications
~~~~~~~~

The class definition for **HTMLstream** is fairly simple:

{lang="lisp"}
~~~~~~~~
(defclass HTMLstream ()
  ((out :accessor out))
  (:documentation "Provide HTML generation services"))
~~~~~~~~

Here, the class name is **HTMLstream**, the list of super classes is an empty list (), the list of slot specifications contains only one slot specification for the slot named **out** and there is only one class specification: a documentation string. Slots are like instance variables in languages like Java and Smalltalk. Most CLOS classes inherit from at least one super class but we will wait until the next section to see examples of inheritance. There is only one slot (or instance variable) and we define an accessor variable with the same name as the slot name. This is a personal preference of mine to name read/write accessor variables with the same name as the slot.

The method **set-header** initializes the string output stream used internally by an instance of this class. This method uses convenience macro **with-accessors** that binds a local set of local variable to one or more class slot accessors. We will list the entire method then discuss it: 


{lang="lisp"}
~~~~~~~~
(defmethod set-header ((ho HTMLstream) title)
  (with-accessors
      ((out out))
      ho
    (setf out (make-string-output-stream))
    (princ "<HTML><head><title>" out)
    (princ title out)
    (princ "</title></head><BODY>" out)
    (terpri out)))
~~~~~~~~

The first interesting thing to notice about the **defmethod** is the argument list: there are two arguments **ho** and **title** but we are constraining the argument ho to be either a member of the class **HTMLstream** or a subclass of **HTMLstream**. Now, it makes sense that since we are passing an instance of the class **HTMLstream** to this generic function (or method – I use the terms “generic function” and “method” interchangeably) that we would want access to the slot defined for this class. The convenience macro **with-accessors** is exactly what we need to get read and write access to the slot inside a generic function (or method) for this class. In the term **((out out))**, the first out is local variable bound to the value of the slot named out for this instance **ho** of class **HTMLstream**. Inside the **with-accessors** macro, we can now use **setf** to set the slot value to a new string output stream. Note: we have not covered the Common Lisp type **string-output-stream** yet in this book, but we will explain its use on the next page.

By the time a call to the method **set-header** (with arguments of an **HTMLstream** instance and a string title) finishes, the instance has its slot set to a new **string-output-stream** and HTML header information is written to the newly created string output stream. Note: this string output stream is now available for use by any class methods called after **set-header**.

There are several methods defined in the file **src/loving_snippets/HTMLstream.lisp**, but we will look at just four of them: **add-H1**, **add-element**, **add-table**, and **get-html-string**. The remaining methods are very similar to **add-H1** and the reader can read the code in the source file.

As in the method set-header, the method **add-H1** uses the macro with-accessors to access the stream output stream slot as a local variable out. In **add-H1** we use the function **princ** that we discussed in Chapter on Input and Output to write HTML text to the string output stream:

{lang="lisp"}
~~~~~~~~
(defmethod add-H1 ((ho HTMLstream) some-text)
  (with-accessors
   ((out out))
   ho
   (princ "<H1>" out)
   (princ some-text out)
   (princ "</H1>" out)
   (terpri out)))
~~~~~~~~

The method **add-element** is very similar to **add-H1** except the string passed as the second argument element is written directly to the stream output stream slot:

{lang="lisp"}
~~~~~~~~
(defmethod add-element ((ho HTMLstream) element)
  (with-accessors
      ((out out))
      ho
    (princ element out)
    (terpri out)))
~~~~~~~~

The method **add-table** converts a list of lists into an HTML table. The Common Lisp function **princ-to-string** is a useful utility function for writing the value of any variable to a string. The functions **string-left-trim** and **string-right-trim** are string utility functions that take two arguments: a list of characters and a string and respectively remove these characters from either the left or right side of a string. Note: another similar function that takes the same arguments is **string-trim** that removes characters from both the front (left) and end (right) of a string. All three of these functions do not modify the second string argument; they return a new string value. Here is the definition of the **add-table** method:

{lang="lisp"}
~~~~~~~~
(defmethod add-table ((ho HTMLstream) table-data)
  (with-accessors
      ((out out))
      ho
    (princ "<TABLE BORDER=\"1\" WIDTH=\"100\%\">" out)
    (dolist (d table-data)
      (terpri out)
      (princ "  <TR>" out)
      (terpri out)
      (dolist (w d)
        (princ "    <TD>" out)
        (let ((str (princ-to-string w)))
          (setq str (string-left-trim '(#\() str))
          (setq str (string-right-trim '(#\)) str))
          (princ str out))
        (princ "</TD>" out)
        (terpri out))
      (princ "  </TR>" out)
      (terpri out))
    (princ "</TABLE>" out)
    (terpri out)))
~~~~~~~~

The method **get-html-string** gets the string stored in the string output stream slot by using the function **get-output-stream-string**:

{lang="lisp"}
~~~~~~~~
(defmethod get-html-string ((ho HTMLstream))
  (with-accessors
      ((out out))
      ho
  (princ "</BODY></HTML>" out)
  (terpri out)
  (get-output-stream-string out)))
~~~~~~~~

CLOS is a rich framework for object oriented programming, providing a superset of features found in languages like Java, Ruby, and Smalltalk. I have barely scratched the surface in this short CLOS example for generating HTML. Later in the book, whenever you see calls to **make-instance**, that lets you know we are using CLOS even if I don't specifically mention CLOS in the examples.

## Using Defstruct or CLOS

You might notice from my own code that I use Common Lisp **defstruct** macros to define data structures more often than I use CLOS. The **defclass** macro used to create CLOS classes are much more flexible but for simple data structures I find that using **defstruct** is much more concise. In the simplest case, a **defstruct** can just be a name of the new type followed by slot names. For each slot like **my-slot-1** accessor functions are generated automatically. Here is a simple example:

{lang="lisp"}
~~~~~~~~
$ ccl
Clozure Common Lisp Version 1.12  DarwinX8664
? (defstruct struct1 s1 s2)
STRUCT1
? (make-struct1 :s1 1 :s2 2)
#S(STRUCT1 :S1 1 :S2 2)
? (struct1-s1 (make-struct1 :s1 1 :s2 2))
1
~~~~~~~~

We defined a struct **struct1** on line3 with two slots names **s1** and **s2**, show the use of the automatically generated constructor **make-struct1** on line 5, and one of the two automatically generated accessor functions **struct1-s1** on line 7. The names of accessor functions are formed with the structure name and the slot name.