# Common Lisp Basics

The material in this chapter will serve as an introduction to Common Lisp. I have attempted to make this book a self contained resource for learning Common Lisp and to provide code examples to perform common tasks. If you already know Common Lisp and bought this book for the code examples later in this book then you can probably skip this chapter.

For working through this chapter we will be using the interactive shell, or **repl**, built into SBCL and other Common Lisp systems. For this chapter it is sufficient for you to [download and install SBCL](http://www.sbcl.org/platform-table.html). Please install SBCL right now, if you have not already done so.

## Getting Started with SBCL

When we start SBCL, we see an introductory message and then an input prompt. We will start with a short tutorial, walking you through a session using SBCL repl (other Common LISP systems are very similar). A repl is an interactive console where you type expressions and see the results of evaluating these expressions. An expression can be a large block of code pasted into the repl, using the **load** function to load Lisp code into the repl, calling functions to test them, etc. Assuming that SBCL is installed on your system, start SBCL by running the SBCL program:

~~~~~~~~
% sbcl
(running SBCL from: /Users/markw/sbcl)
This is SBCL 2.0.2, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

* (defvar x 1.0)

X
* x

1.0
* (+ x 1)

2.0
* x

1.0
* (setq x (+ x 1))

2.0
* x

2.0
* (setq x "the dog chased the cat")

"the dog chased the cat"
* x

"the dog chased the cat"
* (quit)
~~~~~~~~

We started by defining a new variable x in line 11. Notice how the value of the **defvar** macro is the symbol that is defined. The Lisp reader prints **X** capitalized because symbols are made upper case (we will look at the exception later).

In Lisp, a variable can reference any data type. We start by assigning a floating point value to the variable **x**, using the **+** function to add 1 to **x** in line 17, using the **setq** function to change the value of **x** in lines 23 and 29 first to another floating point value and finally setting **x** to a string value. One thing that you will have noticed: function names always occur first, then the arguments to a function. Also, parenthesis is used to separate expressions.

I learned to program Lisp in 1976 and my professor half-jokingly told us that Lisp was an acronym for “Lots-of Irritating Superfluous Parenthesis.” There may be some truth in this when you are just starting with Lisp programming, but you will quickly get used to the parenthesis, especially if you use an editor like Emacs that automatically indents Lisp code for you and highlights the opening parenthesis for every closing parenthesis that you type. Many other editors support coding in Lisp but I personally use Emacs or sometimes VScode (with Common Lisp plugins) to edit Lisp code.

Before you proceed to the next chapter, please take the time to install SBCL on your computer and try typing some expressions into the Lisp listener. If you get errors, or want to quit, try using the quit function:

~~~~~~~~
* (+ 1 2 3 4)

10
* (quit)
Bye.
~~~~~~~~

If you get an error you can enter **help** to get options for handling an error. When I get an error and have a good idea of what caused the error then I just enter **:a:** to abort out of the error).

As we discussed in the introduction, there are many different Lisp programming environments that you can choose from. I recommend a free set of tools: Emacs, Quicklisp, slime, and SBCL. Emacs is a fine text editor that is extensible to work well with many programming languages and document types (e.g., HTML and XML). Slime is an Emacs extension package that greatly facilitates Lisp development. SBCL is a robust Common Lisp compiler and runtime system that is often used in production.

We will cover the Quicklisp package manager and using Quicklisp to setup Slime and Emacs in a later chapter. 

I will not spend much time covering the use of Emacs as a text editor in this book since you can try most of the example code snippets in the book text by copying and then pasting them into a SBCL repl and by loading the book example source files directly into a repl. If you already use Emacs then I recommend that you do set up Slime sooner rather than later and start using it for development. If you are not already an Emacs user and do not mind spending the effort to learn Emacs, then search the web first for an Emacs tutorial. That said, you will easily be able to use the example code from this book using any text editor you like with a SBCL repl. I don't use the vi or vim editors but if vi is your weapon of choice for editing text then a web search for "common lisp vi vim repl" should get you going for developing Common Lisp code with vi or vim. If you are not already an Emacs or vi user then using VSCode with a Common Lisp plugin is recommended.

Here, we will assume that under Windows, Unix, Linux, or Mac OS X you will use one command window to run SBCL and a separate editor that can edit plain text files.

## Making the repl Nicer using rlwrap

While reading the last section you (hopefully!) played with the SBCL interactive repl. If you haven't played with the repl, I won't get too judgmental except to say that if you do not play with the examples as you read you will not get the full benefit from this book.

Did you notice that the backspace key does not work in the SBCL repl? The way to fix this is to install the GNU **rlwrap** utility. On OS X, assuming that you have [homebrew](http://mxcl.github.io/homebrew/) installed, install rlwrap with:

    brew install rlwrap

If you are running Ubuntu Linux, install rlwrap using:

    sudo apt-get install rlwrap

You can then create an alias for bash or zsh using something like the following to define a command **rsbcl**:

    alias rsbcl='rlwrap sbcl'

This is fine, just remember to run **sbcl** if you don't need rlwrap command line editing or run **rsbcl** when you do need command line editing. That said, I find that I *always* want to run SBCL with command line editing, so I redefine *sbcl* on my computers using:

~~~~~~~~
->  ~  which sbcl
/Users/markw/sbcl/sbcl
->  ~  alias sbcl='rlwrap /Users/markw/sbcl/sbcl'
~~~~~~~~

This alias is different on my laptops and servers, since I don't usually install SBCL in the default installation directory. For each of my computers, I add an appropriate alias in my .zshrc file (if I am running zsh) or my .bashrc file (if I am running bash).


## The Basics of Lisp Programming

Although we will use SBCL in this book, any Common Lisp environment will do fine. In previous sections, we saw the top-level Lisp prompt and how we could type any expression that would be evaluated:

~~~~~~~~
* 1
1
* 3.14159
3.14159
* "the dog bit the cat"
"the dog bit the cat"
* (defun my-add-one (x)
(+ x 1))
MY-ADD-ONE
* (my-add-one -10)
-9
~~~~~~~~

Notice that when we defined the function my-add-one in lines 7 and 8, we split the definition over two lines and on line 8 you don't see the "*" prompt from SBCL -- this lets you know that you have not yet entered a complete expression. The top level Lisp evaluator counts parentheses and considers a form to be complete when the number of closing parentheses equals the number of opening parentheses and an expression is complete when the parentheses match. I tend to count in my head, adding one for every opening parentheses and subtracting one for every closing parentheses -- when I get back down to zero then the expression is complete. When we evaluate a number (or a variable), there are no parentheses, so evaluation proceeds when we hit a new line (or carriage return).

The Lisp reader by default tries to evaluate any form that you enter. There is a reader macro **'** that prevents the evaluation of an expression. You can either use the **'** character or **quote**:

~~~~~~~~
* (+ 1 2)
3
* '(+ 1 2)
(+ 1 2)
* (quote (+ 1 2))
(+ 1 2)
* 
~~~~~~~~

Lisp supports both global and local variables. Global variables can be declared using **defvar**:

~~~~~~~~
* (defvar *x* "cat")
*X*
* *x*
"cat"
* (setq *x* "dog")
"dog"
* *x*
"dog"
* (setq *x* 3.14159)
3.14159
* *x*
3.14159
~~~~~~~~

One thing to be careful of when defining global variables with **defvar**: the declared global variable is dynamically scoped. We will discuss dynamic versus lexical scoping later, but for now a warning: if you define a global variable avoid redefining the same variable name inside functions. Lisp programmers usually use a global variable naming convention of beginning and ending dynamically scoped global variables with the \* character. If you follow this naming convention and also do not use the \* character in local variable names, you will stay out of trouble. For convenience, I do not always follow this convention in short examples in this book.

Lisp variables have no type. Rather, values assigned to variables have a type. In this last example, the variable x was set to a string, then to a floating-point number. Lisp types support inheritance and can be thought of as a hierarchical tree with the type t at the top. (Actually, the type hierarchy is a DAG, but we can ignore that for now.) Common Lisp also has powerful object oriented programming facilities in the Common Lisp Object System (CLOS) that we will discuss in a later chapter.

Here is a partial list of types (note that indentation denotes being a subtype of the preceding type):

~~~~~~~~
t  [top level type (all other types are a sub-type)]
     sequence
          list
          array
               vector
                    string
     number
          float
          rational
               integer
               ratio
          complex
     character
     symbol
     structure
     function
     hash-table
~~~~~~~~

We can use the **typep** function to test the type of value of any variable or expression or use **type-of** to get type information of any value):

~~~~~~~~
* (setq x '(1 2 3))
(1 2 3)
* (typep x 'list)
T
* (typep x 'sequence)
T
* (typep x 'number)
NIL
* (typep (+ 1 2 3) 'number)
T
* (type-of 3.14159)
single-float
* (type-of "the dog ran quickly")
(simple-array character (19))
* (type-of 100193)
(integer 0 4611686018427387903)
~~~~~~~~

A useful feature of all ANSI standard Common Lisp implementations' top-level listener is that it sets \* to the value of the last expression evaluated. For example:

~~~~~~~~
* (+ 1 2 3 4 5)
15
* *
15
* (setq x *)
15
* x
15
~~~~~~~~

All Common Lisp environments set * to the value of the last expression evaluated. This example may be slightly confusing because * is also the prompt character in the SBCL repl that indicates that you can enter a new expression for evaluation. For example in line 3, the first * character is the repl prompt and the second * we type in to see that value of the previous expression that we typed into the repl.

Frequently, when you are interactively testing new code, you will call a function that you just wrote with test arguments; it is useful to save intermediate results for later testing. It is the ability to create complex data structures and then experiment with code that uses or changes these data structures that makes Lisp programming environments so effective.

Common Lisp is a lexically scoped language that means that variable declarations and function definitions can be nested and that the same variable names can be used in nested let forms; when a variable is used, the current let form is searched for a definition of that variable and if it is not found, then the next outer let form is searched. Of course, this search for the correct declaration of a variable is done at compile time so there need not be extra runtime overhead. We should not nest **defun** special form inside each other or inside **let** expressions. Instead we use the special forms **flet** and **labels** to define functions inside a scoped environment. Functions defined inside a **labels** special form can be recursive while functions defined inside a **flet** special form cannot be recursive. Consider the following example in the file **nested.lisp** (all example files are in the **src** directory):

{lang="lisp",linenos=on}
~~~~~~~~
(flet ((add-one (x)
         (+ x 1))
       (add-two (x)
         (+ x 2)))
  (format t "redefined variables: ~A  ~A~%" (add-one 100) (add-two 100)))

(let ((a 3.14))
  (defun test2 (x) ; this works, but don't do it!
    (print x))
  (test2 a))

(test2 50)

(let ((x 1)
      (y 2))
  ;; properly define a test function nested inside a let statement:
  (flet ((test (a b)
           (let ((z (+ a b)))
             ;; define a helper function nested inside a let/function/let:
             (flet ((nested-function (a)
                      (+ a a)))
               (nested-function z)))))
    ;; call nested function 'test':
    (format t "test result is ~A~%" (test x y))))

(let ((z 10))
  (labels ((test-recursion (a)
             (format t "test-recursion ~A~%" (+ a z))
             (if (> a 0)
                 (test-recursion (- a 1)))))
    (test-recursion 5)))
~~~~~~~~

We define a top level **flet** special form in lines 1-5 that defines two nested functions **add-one** and **add-two** and then calls each nested function in the body of the **flet** special form. For many years I have used nested **defun** special forms inside **let** expressions for defining local functions but I now try to avoid doing this. Functions defined inside **defun** special forms have global visibility so they are not hidden in the local context where they are defined. The example of a nested **defun** in lines 7-12 shows that the function **test2** has global visibility inside the current package.

Functions defined inside of a **flet** special form have access to variables defined in the outer scope containing the **flet** (also applies to **labels**). We see this in lines 14-24 where the local variables **x** and **y** defined in the **let** expression are visible inside the function **nested-function** defined inside the **flet**.

The final example in lines 26-31 shows a recursive function defined inside a **labels** special form.

Assuming that we started SBCL in the **src** directory we can then use the Lisp load function to evaluate the contents of the file **nested.lisp** in the sub-directory **code_snippets_for_book** using the **load** function:

{linenos=off}
~~~~~~~~
* (load "./code_snippets_for_book/nested.lisp")
redefined variables: 101  102

3.14 
50 test result is 6
test-recursion 15
test-recursion 14
test-recursion 13
test-recursion 12
test-recursion 11
test-recursion 10
T
*
~~~~~~~~

The function **load** returned a value of **t** (prints in upper case as **T**) after successfully loading the file.

We will use Common Lisp vectors and arrays frequently in later chapters, but will also briefly introduce them here. A singly dimensioned array is also called a vector. Although there are often more efficient functions for handling vectors, we will just look at generic functions that handle any type of array, including vectors. Common Lisp provides support for functions with the same name that take different argument types; we will discuss this in some detail when we cover this in the later chapter on CLOS. We will start by defining three vectors **v1**, **v2**, and **v3**:

{linenos=on}
~~~~~~~~
* (setq v1 (make-array '(3)))
#(NIL NIL NIL)
* (setq v2 (make-array '(4) :initial-element "lisp is good"))
#("lisp is good" "lisp is good" "lisp is good" "lisp is good")
* (setq v3 #(1 2 3 4 "cat" '(99 100)))
#(1 2 3 4 "cat" '(99 100))
~~~~~~~~

In line 1, we are defining a one-dimensional array, or vector, with three elements. In line 3 we specify the default value assigned to each element of the array *v2*. In line 5 I use the form for specifying array literals using the special character \#. The function **aref** can be used to access any element in an array:

{linenos=off}
~~~~~~~~
* (aref v3 3)
4
* (aref v3 5)
'(99 100)
* 
~~~~~~~~

Notice how indexing of arrays is zero-based; that is, indices start at zero for the first element of a sequence. Also notice that array elements can be any Lisp data type. So far, we have used the special operator **setq** to set the value of a variable. Common Lisp has a generalized version of **setq** called **setf** that can set any value in a list, array, hash table, etc. You can use **setf** instead of **setq** in all cases, but not vice-versa. Here is a simple example:

{linenos=off}
~~~~~~~~
* v1
#(NIL NIL NIL)
* (setf (aref v1 1) "this is a test") 
"this is a test"
* v1
#(NIL "this is a test" NIL)
* 
~~~~~~~~

When writing new code or doing quick programming experiments, it is often easiest (i.e., quickest to program) to use lists to build interesting data structures. However, as programs mature, it is common to modify them to use more efficient (at runtime) data structures like arrays and hash tables.

## Symbols

We will discuss symbols in more detail the [Chapter on Common Lisp Packages](#package_system). For now, it is enough for you to understand that symbols can be names that refer to variables. For example:

{linenos=off}
~~~~~~~~
> (defvar *cat* "bowser")
*CAT*
* *cat*
"bowser"
* (defvar *l* (list *cat*))
*L*
* *l*
("bowser")
*
~~~~~~~~

Note that the first **defvar** returns the defined symbol as its value. Symbols are almost always converted to upper case. An exception to this "upper case rule" is when we define symbols that may contain white space using vertical bar characters:

{linenos=off}
~~~~~~~~
* (defvar |a symbol with Space Characters| 3.14159)
|a symbol with Space Characters|
* |a symbol with Space Characters|
3.14159
* 
~~~~~~~~


## Operations on Lists

Lists are a fundamental data structure of Common Lisp. In this section, we will look at some of the more commonly used functions that operate on lists. All of the functions described in this section have something in common: they do not modify their arguments.

In Lisp, a cons cell is a data structure containing two pointers. Usually, the first pointer in a cons cell will point to the first element in a list and the second pointer will point to another cons representing the start of the rest of the original list.

The function cons takes two arguments that it stores in the two pointers of a new cons data structure. For example:

{linenos=off}
~~~~~~~~
* (cons 1 2)
(1 . 2)
* (cons 1 '(2 3 4))
(1 2 3 4)
* 
~~~~~~~~

The first form evaluates to a cons data structure while the second evaluates to a cons data structure that is also a proper list. The difference is that in the second case the second pointer of the freshly created cons data structure points to another cons cell.

First, we will declare two global variables **l1** and **l2** that we will use in our examples. The list **l1** contains five elements and the list **l2** contains four elements:

{linenos=off}
~~~~~~~~
* (defvar l1 '(1 2 (3) 4 (5 6)))
L1
* (length l1)

5
* (defvar l2 '(the "dog" calculated 3.14159))
L2
* l1
(1 2 (3) 4 (5 6))
* l2
(THE "dog" CALCULATED 3.14159)
>
~~~~~~~~

You can also use the function **list** to create a new list; the arguments passed to function list are the elements of the created list:

{linenos=off}
~~~~~~~~
* (list 1 2 3 'cat "dog")
(1 2 3 CAT "dog")
*
~~~~~~~~

The function **car** returns the first element of a list and the function **cdr** returns a list with its first element removed (but does not modify its argument):

{linenos=off}
~~~~~~~~
* (car l1)
1
* (cdr l1)
(2 (3) 4 (5 6))
*
~~~~~~~~

Using combinations of **car** and **cdr** calls can be used to extract any element of a list:

{linenos=off}
~~~~~~~~
* (car (cdr l1))
2
* (cadr l1)
2
*
~~~~~~~~

Notice that we can combine calls to **car** and **cdr** into a single function call, in this case the function **cadr**. Common Lisp defines all functions of the form **cXXr**, **cXXXr**, and **cXXXXr** where **X** can be either **a** or **d**.

Suppose that we want to extract the value 5 from the nested list **l1**. Some experimentation with using combinations of **car** and **cdr** gets the job done:

{linenos=off}
~~~~~~~~
* l1
(1 2 (3) 4 (5 6))
* (cadr l1)
2
* (caddr l1)
(3)
(car (caddr l1))
3
* (caar (last l1))
5
* (caar (cddddr l1))

5
*
~~~~~~~~

The function **last** returns the last **cdr** of a list (i.e., the last element, in a list):

{linenos=off}
~~~~~~~~
* (last l1)
((5 6))
*
~~~~~~~~

Common list supplies alternative functions to **car** and **cdr** that you might find more readable: **first**, **second**, **third**, **fourth**, and **rest**. Here are some examples:

{linenos=off}
~~~~~~~~
* (defvar *x* '(1 2 3 4 5))

*X*
* (first *x*)

1
* (rest *x*)

(2 3 4 5)
* (second *x*)

2
* (third *x*)

3
* (fourth *x*)

4
~~~~~~~~


The function **nth** takes two arguments: an index of a top-level list element and a list. The first index argument is zero based:

{linenos=off}
~~~~~~~~
* l1
(1 2 (3) 4 (5 6))
* (nth 0 l1)
1
* (nth 1 l1)
2
* (nth 2 l1)
(3)
*
~~~~~~~~

The function **cons** adds an element to the beginning of a list and returns as its value a new list (it does not modify its arguments). An element added to the beginning of a list can be any Lisp data type, including another list:

{linenos=off}
~~~~~~~~
* (cons 'first l1)
(FIRST 1 2 (3) 4 (5 6))
* (cons '(1 2 3) '(11 22 33))
((1 2 3) 11 22 33)
* 
~~~~~~~~

The function **append** takes two lists as arguments and returns as its value the two lists appended together:

{linenos=off}
~~~~~~~~
* l1
(1 2 (3) 4 (5 6))
* l2
('THE "dog" 'CALCULATED 3.14159)
* (append l1 l2)
(1 2 (3) 4 (5 6) THE "dog" CALCULATED 3.14159)
* (append '(first) l1)
(FIRST 1 2 (3) 4 (5 6))
* 
~~~~~~~~

A frequent error that beginning Lisp programmers make is not understanding shared structures in lists. Consider the following example where we generate a list y by reusing three copies of the list x:

{linenos=off}
~~~~~~~~
* (setq x '(0 0 0 0))
(0 0 0 0)
* (setq y (list x x x))
((0 0 0 0) (0 0 0 0) (0 0 0 0))
* (setf (nth 2 (nth 1 y)) 'x)
X
* x
(0 0 X 0)
* y
((0 0 X 0) (0 0 X 0) (0 0 X 0))
* (setq z '((0 0 0 0) (0 0 0 0) (0 0 0 0)))
((0 0 0 0) (0 0 0 0) (0 0 0 0))
* (setf (nth 2 (nth 1 z)) 'x)
X
* z
((0 0 0 0) (0 0 X 0) (0 0 0 0))
* 
~~~~~~~~

When we change the shared structure referenced by the variable **x** that change is reflected three times in the list **y**. When we create the list stored in the variable **z** we are not using a shared structure.



## Using Arrays and Vectors

Using lists is easy but the time spent accessing a list element is proportional to the length of the list. Arrays and vectors are more efficient at runtime than long lists because list elements are kept on a linked-list that must be searched. Accessing any element of a short list is fast, but for sequences with thousands of elements, it is faster to use vectors and arrays.

By default, elements of arrays and vectors can be any Lisp data type. There are options when creating arrays to tell the Common Lisp compiler that a given array or vector will only contain a single data type (e.g., floating point numbers) but we will not use these options in this book.

Vectors are a specialization of arrays; vectors are arrays that only have one dimension. For efficiency, there are functions that only operate on vectors, but since array functions also work on vectors, we will concentrate on arrays. In the next section, we will look at character strings that are a specialization of vectors.

We could use the generalized **make-sequence** function to make a singularly dimensioned array (i.e., a vector). Restart sbcl and try:

{linenos=off}
~~~~~~~~
* (defvar x (make-sequence 'vector 5 :initial-element 0))
X
* x
#(0 0 0 0 0)
* 
~~~~~~~~

In this example, notice the print format for vectors that looks like a list with a proceeding # character. As seen in the last section, we use the function **make-array** to create arrays:

{linenos=off}
~~~~~~~~
* (defvar y (make-array '(2 3) :initial-element 1))
Y
* y
#2A((1 1 1) (1 1 1))
>
~~~~~~~~

Notice the print format of an array: it looks like a list proceeded by a # character and the integer number of dimensions.

Instead of using **make-sequence** to create vectors, we can pass an integer as the first argument of **make-array** instead of a list of dimension values. We can also create a vector by using the function **vector** and providing the vector contents as arguments:

{linenos=off}
~~~~~~~~
* (make-array 10)  
#(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
* (vector 1 2 3 'cat)
#(1 2 3 CAT)
* 
~~~~~~~~

The function **aref** is used to access sequence elements. The first argument is an array and the remaining argument(s) are array indices. For example:

{linenos=off}
~~~~~~~~
* x
#(0 0 0 0 0)
* (aref x 2)
0
* (setf (aref x 2) "parrot")
"parrot"
* x
#(0 0 "parrot" 0 0)
* (aref x 2)
"parrot"
* y
#2A((1 1 1) (1 1 1))
* (setf (aref y 1 2) 3.14159)
3.14159
* y
#2A((1 1 1) (1 1 3.14159))
* 
~~~~~~~~


## Using Strings

It is likely that even your first Lisp programs will involve the use of character strings. In this section, we will cover the basics: creating strings, concatenating strings to create new strings, for substrings in a string, and extracting substrings from longer strings. The string functions that we will look at here do not modify their arguments; rather, they return new strings as values. For efficiency, Common Lisp does include destructive string functions that do modify their arguments but we will not discuss these destructive functions here.

We saw earlier that a string is a type of vector, which in turn is a type of array (which in turn is a type of sequence). A full coverage of the Common Lisp type system is outside the scope of this tutorial introduction to Common Lisp; a very good treatment of Common Lisp types is in Guy Steele's "Common Lisp, The Language" which is available both in print and for free on the web. Many of the built in functions for handling strings are actually more general because they are defined for the type sequence. The Common Lisp Hyperspec is another great free resource that you can find on the web. I suggest that you download an HTML version of Guy Steele's excellent reference book and the Common Lisp Hyperspec and keep both on your computer. If you continue using Common Lisp, eventually you will want to read all of Steele's book and use the Hyperspec for reference.

The following text was captured from input and output from a Common Lisp repl. First, we will declare two global variables **s1** and **space** that contain string values:

{linenos=off}
~~~~~~~~
* (defvar s1 "the cat ran up the tree")
S1
* (defvar space " ")
SPACE
* 
~~~~~~~~

One of the most common operations on strings is to concatenate two or more strings into a new string:

{linenos=off}
~~~~~~~~
* (concatenate 'string s1 space "up the tree")
"the cat ran up the tree up the tree"
*
~~~~~~~~

Notice that the first argument of the function **concatenate** is the type of the sequence that the function should return; in this case, we want a string. Another common string operation is search for a substring:

{linenos=off}
~~~~~~~~
* (search "ran" s1)
8
* (search "zzzz" s1)
NIL
*
~~~~~~~~

If the search string (first argument to function search) is not found, function search returns nil, otherwise search returns an index into the second argument string. Function **search** takes several optional keyword arguments (see the next chapter for a discussion of keyword arguments):

{lang="lisp",linenos=off}
~~~~~~~~
  (search search-string a-longer-string :from-end :test
                                        :test-not :key
                                        :start1 :start2
                                        :end1 :end2)
~~~~~~~~

For our discussion, we will just use the keyword argument **:start2** for specifying the starting search index in the second argument string and the **:from-end** flag to specify that search should start at the end of the second argument string and proceed backwards to the beginning of the string:

{linenos=off}
~~~~~~~~
* (search " " s1)
3
* (search " " s1 :start2 5)
7
* (search " " s1 :from-end t)
18
*
~~~~~~~~

The sequence function **subseq** can be used for strings to extract a substring from a longer string:

{linenos=off}
~~~~~~~~
* (subseq s1 8)
"ran up the tree"
>
~~~~~~~~

Here, the second argument specifies the starting index; the substring from the starting index to the end of the string is returned. An optional third index argument specifies one greater than the last character index that you want to extract:

{linenos=off}
~~~~~~~~
* (subseq s1 8 11)
"ran"
*
~~~~~~~~

It is frequently useful to remove white space (or other) characters from the beginning or end of a string:

{linenos=off}
~~~~~~~~
* (string-trim '(#\space #\z #\a) " a boy said pez")
"boy said pe"
* 
~~~~~~~~

The character #\space is the space character. Other common characters that are trimmed are #\tab and #\newline. There are also utility functions for making strings upper or lower case:

{linenos=off}
~~~~~~~~
* (string-upcase "The dog bit the cat.")
"THE DOG BIT THE CAT."
* (string-downcase "The boy said WOW!")
"the boy said wow!"
>
~~~~~~~~

We have not yet discussed equality of variables. The function **eq** returns true if two variables refer to the same data in memory. The function **eql** returns true if the arguments refer to the same data in memory or if they are equal numbers or characters. The function **equal** is more lenient: it returns true if two variables print the same when evaluated. More formally, function **equal** returns true if the **car** and **cdr** recursively equal to each other. An example will make this clearer:

{linenos=off}
~~~~~~~~
* (defvar x '(1 2 3))
X
* (defvar y '(1 2 3))
Y
* (eql x y)
NIL
* (equal x y)
T
* x
(1 2 3)
* y
(1 2 3)
* 
~~~~~~~~

For strings, the function **string=** is slightly more efficient than using the function **equal**:

{linenos=off}
~~~~~~~~
* (eql "cat" "cat")
NIL
* (equal "cat" "cat")
T
* (string= "cat" "cat")
T
* 
~~~~~~~~

Common Lisp strings are sequences of characters. The function **char** is used to extract individual characters from a string:

{linenos=off}
~~~~~~~~
* s1
"the cat ran up the tree"
* (char s1 0)
#\t
* (char s1 1)
#\h
* 
~~~~~~~~



## Using Hash Tables

Hash tables are an extremely useful data type. While it is true that you can get the same effect by using lists and the **assoc** function, hash tables are much more efficient than lists if the lists contain many elements. For example:

{linenos=off}
~~~~~~~~
* (defvar x '((1 2) ("animal" "dog")))
X
* (assoc 1 x)
(1 2)
* (assoc "animal" x)
NIL
* (assoc "animal" x :test #'equal)
("animal" "dog")
*
~~~~~~~~

The second argument to function **assoc** is a list of cons cells. Function **assoc** searches for a sub-list (in the second argument) that has its **car** (i.e., first element) equal to the first argument to function **assoc**. The perhaps surprising thing about this example is that **assoc** seems to work with an integer as the first argument but not with a string. The reason for this is that by default the test for equality is done with **eql** that tests two variables to see if they refer to the same memory location or if they are identical if they are numbers. In the last call to **assoc** we used ":test #'equal" to make **assoc** use the function **equal** to test for equality.

The problem with using lists and **assoc** is that they are very inefficient for large lists. We will see that it is no more difficult to code with hash tables.

A hash table stores associations between key and value pairs, much like our last example using the **assoc** function. By default, hash tables use **eql** to test for equality when looking for a key match.  We will duplicate the previous example using hash tables:

{linenos=off}
~~~~~~~~
* (defvar h (make-hash-table))
H
* (setf (gethash 1 h) 2)
2
* (setf (gethash "animal" h) "dog")
"dog"
* (gethash 1 h)
2 ;
T
* (gethash "animal" h)
NIL ;
NIL
*
~~~~~~~~

Notice that **gethash** returns multiple values: the first value is the value matching the key passed as the first argument to function **gethash** and the second returned value is true if the key was found and nil otherwise. The second returned value could be useful if hash values are nil.

Since we have not yet seen how to handle multiple returned values from a function, we will digress and do so here (there are many ways to handle multiple return values and we are just covering one of them):

{linenos=off}
~~~~~~~~
* (multiple-value-setq (a b) (gethash 1 h))
2
* a
2
* b
T
* 
~~~~~~~~

Assuming that variables **a** and **b** are already declared, the variable **a** will be set to the first returned value from **gethash** and the variable **b** will be set to the second returned value.

If we use symbols as hash table keys, then using **eql** for testing for equality with hash table keys is fine:

{linenos=off}
~~~~~~~~
* (setf (gethash 'bb h) 'aa)
AA
* (gethash 'bb h)
AA ;
T
* 
~~~~~~~~

However, we saw that **eql** will not match keys with character string values. The function **make-hash-table** has optional key arguments and one of them will allow us to use strings as hash key values:

{linenos=off}
~~~~~~~~
  (make-hash-table &key :test :size :rehash-size :rehash-threshold)
~~~~~~~~

Here, we are only interested in the first optional key argument :test that allows us to use the function equal to test for equality when matching hash table keys. For example:

{linenos=off}
~~~~~~~~
* (defvar h2 (make-hash-table :test #'equal))
H2
* (setf (gethash "animal" h2) "dog")
"dog"
* (setf (gethash "parrot" h2) "Brady")
"Brady"
* (gethash "parrot" h2)
"Brady" ;
T
* 
~~~~~~~~

It is often useful to be able to enumerate all the key and value pairs in a hash table. Here is a simple example of doing this by first defining a function **my-print** that takes two arguments, a key and a value. We can then use the **maphash** function to call our new function **my-print** with every key and value pair in a hash table:

{linenos=off}
~~~~~~~~
* (defun my-print (a-key a-value)
        (format t "key: ~A value: ~A~\%" a-key a-value))          
MY-PRINT
* (maphash #'my-print h2)
key: parrot value: Brady
key: animal value: dog
NIL
* 
~~~~~~~~

The function **my-print** is applied to each key/value pair in the hash table. There are a few other useful hash table functions that we demonstrate here:

{linenos=off}
~~~~~~~~
* (hash-table-count h2)
2
* (remhash "animal" h2)
T
* (hash-table-count h2)
1
* (clrhash h2)
#S(HASH-TABLE EQUAL)
* (hash-table-count h2)
0
* 
~~~~~~~~

The function **hash-table-count** returns the number of key and value pairs in a hash table. The function **remhash** can be used to remove a single key and value pair from a hash table. The function **clrhash** clears out a hash table by removing all key and value pairs in a hash table.

It is interesting to note that **clrhash** and **remhash** are the first Common Lisp functions that we have seen so far that modify any of its arguments, except for **setq** and **setf** that are macros and not functions.



## Using Eval to Evaluate Lisp Forms

We have seen how we can type arbitrary Lisp expressions in the Lisp repl listener and then they are evaluated. We will see in the [Chapter on Input and Output](#input_output) that the Lisp function **read** evaluates lists (or forms) and indeed the Lisp repl uses function read.

In this section, we will use the function **eval** to evaluate arbitrary Lisp expressions inside a program. As a simple example:

{linenos=off}
~~~~~~~~
* (defvar x '(+ 1 2 3 4 5))
X
* x
(+ 1 2 3 4 5)
* (eval x)
15
* 
~~~~~~~~

Using the function **eval**, we can build lists containing Lisp code and evaluate generated code inside our own programs. We get the effect of "data is code". A classic Lisp program, the OPS5 expert system tool, stored snippets of Lisp code in a network data structure and used the function eval to execute Lisp code stored in the network. A warning: the use of **eval** is likely to be inefficient in non-compiled code. For efficiency, the OPS5 program contained its own version of **eval** that only interpreted a subset of Lisp used in the network.


## Using a Text Editor to Edit Lisp Source Files

I usually use Emacs, but we will briefly discuss the editor vi also. If you use vi (e.g., enter “vi nested.lisp”) the first thing that you should do is to configure vi to indicate matching opening parentheses whenever a closing parentheses is typed; you do this by typing “:set sm” after vi is running.

If you choose to learn Emacs, enter the following in your .emacs file (or your _emacs file in your home directory if you are running Windows): 

{lang="lisp",linenos=on}
~~~~~~~~
  (set-default 'auto-mode-alist
               (append '(("\\.lisp$" . lisp-mode)
                         ("\\.lsp$" . lisp-mode)
                         ("\\.cl$" . lisp-mode))
                       auto-mode-alist))
~~~~~~~~

Now, whenever you open a file with the extension of “lisp”, “lsp”, or “cl” (for “Common Lisp”) then Emacs will automatically use a Lisp editing mode. I recommend searching the web using keywords “Emacs tutorial” to learn how to use the basic Emacs editing commands - we will not repeat this information here.

I do my professional Lisp programming using free software tools: Emacs, SBCL, Clozure Common Lisp, and Clojure. I will show you how to configure Emacs and Slime in the last section of the [Chapter on Quicklisp](#quicklisp).


## Recovering from Errors

When you enter forms (or expressions) in a Lisp repl listener, you will occasionally make a mistake and an error will be thrown. Here is an example where I am not showing all of the output when entering **help** when an error is thrown:

{linenos=off}
~~~~~~~~
* (defun my-add-one (x) (+ x 1))

MY-ADD-ONE
* (my-add-one 10)

11
* (my-add-one 3.14159)

4.14159
* (my-add-one "cat")

debugger invoked on a SIMPLE-TYPE-ERROR: Argument X is not a NUMBER: "cat"

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.

(SB-KERNEL:TWO-ARG-+ "cat" 1)
0] help

The debug prompt is square brackets, with number(s) indicating the current
  control stack level and, if you've entered the debugger recursively, how
  deeply recursed you are.

 ...

Getting in and out of the debugger:
  TOPLEVEL, TOP  exits debugger and returns to top level REPL
  RESTART        invokes restart numbered as shown (prompt if not given).
  ERROR          prints the error condition and restart cases.

 ...

Inspecting frames:
  BACKTRACE [n]  shows n frames going down the stack.
  LIST-LOCALS, L lists locals in current frame.
  PRINT, P       displays function call for current frame.
  SOURCE [n]     displays frame's source form with n levels of enclosing forms.

Stepping:
  START Selects the CONTINUE restart if one exists and starts
        single-stepping. Single stepping affects only code compiled with
        under high DEBUG optimization quality. See User Manual for details.
  STEP  Steps into the current form.
  NEXT  Steps over the current form.
  OUT   Stops stepping temporarily, but resumes it when the topmost frame that
        was stepped into returns.
  STOP  Stops single-stepping.

 ...

0] list-locals
SB-DEBUG::ARG-0  =  "cat"
SB-DEBUG::ARG-1  =  1

0] backtrace 2

Backtrace for: #<SB-THREAD:THREAD "main thread" RUNNING {1002AC32F3}>
0: (SB-KERNEL:TWO-ARG-+ "cat" 1)
1: (MY-ADD-ONE "cat")
0] :0

*
~~~~~~~~

Here, I first used the backtrace command **:bt** to print the sequence of function calls that caused the error. If it is obvious where the error is in the code that I am working on then I do not bother using the backtrace command. I then used the abort command **:a** to recover back to the top level Lisp listener (i.e., back to the greater than prompt). Sometimes, you must type **:a** more than once to fully recover to the top level greater than prompt.


## Garbage Collection

Like other languages like Java and Python, Common Lisp provides garbage collection (GC) or automatic memory management.

In simple terms, GC occurs to free memory in a Lisp environment that is no longer accessible by any global variable (or function closure, which we will cover in the next chapter). If a global variable **\*variable-1\*** is first set to a list and then if we later then set **\*variable-1\*** to, for example nil, and if the data referenced in the original list is not referenced by any other accessible data, then this now unused data is subject to GC.

In practice, memory for Lisp data is allocated in time ordered batches and ephemeral or generational garbage collectors garbage collect recent memory allocations far more often than memory that has been allocated for a longer period of time.


##  Loading your Working Environment Quickly

When you start using Common Lisp for large projects, you will likely have many files to load into your Lisp environment when you start working. Most Common Lisp implementations have a function called **defsystem** that works somewhat like the Unix make utility. While I strongly recommend **defsystem** for large multi-person projects, I usually use a simpler scheme when working on my own: I place a file **loadit.lisp** in the top directory of each project that I work on. For any project, its **loadit.lisp** file loads all source files and initializes any global data for the project. 

The last two chapters of this book provide example applications that are configured to work with Quicklisp, which we will study in the next chapter.

Another good technique is to create a Lisp image containing all the code and data for all your projects. There is an example of this in the first section of the [Chapter on NLP](#nlp_chapter). In this example, it takes a few minutes to load the code and data for my NLP (natural language processing) library so when I am working with it I like to be able to quickly load a SBCL Lisp image.

All Common Lisp implementations have a mechanism for dumping a working image containing code and data.


## Functional Programming Concepts

There are two main styles for doing Common Lisp development. Object oriented programming is well supported (see the [Chapter on CLOS](#clos_chapter)) as is functional programming. In a nut shell, functional programming means that we should write functions with no side effects. First let me give you a non-functional example with side effects:

{lang="lisp",linenos=off}
~~~~~~~~
(defun non-functional-example (car)
  (set-color car "red"))
~~~~~~~~

This example using CLOS is non-functional because we modify the value of an argument to the function. Some functional languages like the Lisp Clojure language and the Haskell language dissuade you from modifying arguments to functions. With Common Lisp you should make a decision on which approach you like to use.

Functional programming means that we avoid maintaining state inside of functions and treat data as immutable (i.e., once an object is created, it is never modified). We could modify the last example to be function by creating a new car object inside the function, copy the attributes of the car passed as an object, change the color to "red" of the new car object, and return the new car instance as the value of the function.

Functional programming prevents many types of programming errors, makes unit testing simpler, and makes programming for modern multi-core CPUs easier because read-only objects are inherently thread safe. Modern best practices for the Java language also prefer immutable data objects and a functional approach.

