# Defining Lisp Functions

In the previous chapter, we defined a few simple functions. In this chapter, we will discuss how to write functions that take a variable number of arguments, optional arguments, and keyword arguments.

The special form **defun** is used to define new functions either in Lisp source files or at the top level Lisp listener prompt. Usually, it is most convenient to place function definitions in a source file and use the function **load** to load them into our Lisp working environment.

In general, it is bad form to use global variables inside Lisp functions. Rather, we prefer to pass all required data into a function via its argument list and to get the results of the function as the value (or values) returned from a function. Note that if we do require global variables, it is customary to name them with beginning and ending \* characters; for example:

{lang="lisp",linenos=on}
~~~~~~~~
(defvar *lexical-hash-table*
        (make-hash-table :test #'equal :size 5000))
~~~~~~~~

Then in this example, if you see the variable **\*lexical-hash-table\*** inside a function definition, you will know that at least by naming convention, that this is a global variable.

In Chapter 1, we saw an example of using lexically scoped local variables inside a function definition (in the example file **nested.lisp**).

There are several options for defining the arguments that a function can take. The fastest way to introduce the various options is with a few examples.
 
First, we can use the **&aux** keyword to declare local variables for use in a function definition:

~~~~~~~~
* (defun test (x &aux y)
       (setq y (list x x))
       y)
TEST
* (test 'cat)
(CAT CAT)
* (test 3.14159)
(3.14159 3.14159)
~~~~~~~~

It is considered better coding style to use the **let** special operator for defining auxiliary local variables; for example:

~~~~~~~~
* (defun test (x)
       (let ((y (list x x)))
         y))
TEST
* (test "the dog bit the cat")
("the dog bit the cat" "the dog bit the cat")
* 
~~~~~~~~

You will probably not use **&aux** very often, but there are two other options for specifying function arguments: **&optional** and **&key**.

The following code example shows how to use optional function arguments. Note that optional arguments must occur after required arguments.

~~~~~~~~
* (defun test (a &optional b (c 123))
        (format t "a=~A b=~A c=~A~%" a b c))
TEST
* (test 1)
a=1 b=NIL c=123
NIL
* (test 1 2)
a=1 b=2 c=123
NIL
* (test 1 2 3)
a=1 b=2 c=3
NIL
* (test 1 2 "Italian Greyhound")
a=1 b=2 c=Italian Greyhound
NIL
* 
~~~~~~~~

In this example, the optional argument **b** was not given a default value so if unspecified it will default to nil. The optional argument **c** is given a default value of 123.

We have already seen the use of keyword arguments in built-in Lisp functions. Here is an example of how to specify key word arguments in your functions:

~~~~~~~~
* (defun test (a &key b c)
        (format t "a=~A b=~A c=~A~%" a b c))
TEST
* (test 1)
a=1 b=NIL c=NIL
NIL
* (test 1 :c 3.14159)
a=1 b=NIL c=3.14159
NIL
* (test "cat" :b "dog")
a=cat b=dog c=NIL
NIL
* 
~~~~~~~~

## Using Lambda Forms

It is often useful to define unnamed functions. We can define an unnamed function using **lambda**; for example, let's look at the example file **src/lambda1.lisp**. But first, we will introduce the Common Lisp function **funcall** that takes one or more arguments; the first argument is a function and any remaining arguments are passed to the function bound to the first argument. For example:

~~~~~~~~
* (funcall 'print 'cat)
CAT 
CAT
* (funcall '+ 1 2)
3
* (funcall #'- 2 3)
-1
* 
~~~~~~~~

In the first two calls to **funcall** here, we simply quote the function name that we want to call. In the third example, we use a better notation by quoting with **#'**. We use the **#'** characters to quote a function name.

Consider the following repl listing where we will look at a primary difference between quoting a symbol using **'** and with **#'**:

{lang="lisp",linenos=on}
~~~~~~~~
$ ccl
Clozure Common Lisp Version 1.12  DarwinX8664
? 'barfoo531
BARFOO531
? (apropos "barfoo")
BARFOO531
? #'bar987
> Error: Undefined function: BAR987
~~~~~~~~

On line three we create a new symbol **BARFOO531** that is interned as you can see from looking at all interned symbols containing the string "barfoo". Line 7 throws an error because **#'** does not intern a new symbol.

Here is the example file **src/lambda1.lisp**:

{lang="lisp",linenos=on}
~~~~~~~~
(defun test ()
  (let ((my-func
          (lambda (x) (+ x 1))))
    (funcall my-func 1)))
~~~~~~~~

Here, we define a function using **lambda** and set the value of the local variable **my-func** to the unnamed function's value.  Here is output from the function test:

~~~~~~~~
* (test)
2

* 
~~~~~~~~

The ability to use functions as data is surprisingly useful. For now, we will look at a simple example:

~~~~~~~~
* (defvar f1 #'(lambda (x) (+ x 1)))

F1
* (funcall f1 100)

101
* (funcall #'print 100)

100 
100
~~~~~~~~

Notice that the second call to function **testfn** prints "100" twice: the first time as a side effect of calling the function **print** and the second time as the returned value of **testfn** (the function **print** returns what it is printing as its value).

##  Using Recursion

Later, we will see how to use special Common Lisp macros for programming repetitive loops. In this section, we will use recursion for both coding simple loops and as an effective way to solve a variety of problems that can be expressed naturally using recursion.

As usual, the example programs for this section are found in the **src** directory. In the file **src/recursion1.lisp**, we see our first example of recursion:

{lang="lisp",linenos=on}
~~~~~~~~
;; a simple loop using recursion

(defun recursion1 (value)
  (format t "entering recursion1(~A)~\%" value)
  (if (< value 5)
      (recursion1 (1+ value))))
~~~~~~~~

This example is simple, but it is useful for discussing a few points. First, notice how the function **recursion1** calls itself with an argument value of one greater than its own input argument only if the input argument "value" is less than 5. This test keeps the function from getting in an infinite loop. Here is some sample output:

~~~~~~~~
* (load "recursion1.lisp")
;; Loading file recursion1.lisp ...
;; Loading of file recursion1.lisp is finished.
T
* (recursion1 0)
entering recursion1(0)
entering recursion1(1)
entering recursion1(2)
entering recursion1(3)
entering recursion1(4)
entering recursion1(5)
NIL
* (recursion1 -3)
entering recursion1(-3)
entering recursion1(-2)
entering recursion1(-1)
entering recursion1(0)
entering recursion1(1)
entering recursion1(2)
entering recursion1(3)
entering recursion1(4)
entering recursion1(5)
NIL
* (recursion1 20)
entering recursion1(20)
NIL
* 
~~~~~~~~

Why did the call on line 24 not loop via recursion? Because the input argument is not less than 5, no recursion occurs.

## Closures

We have seen that functions can take other functions as arguments and return new functions as values. A function that references an outer lexically scoped variable is called a *closure*. The example file **src/closure1.lisp** contains a simple example:

{lang="lisp",linenos=on}
~~~~~~~~
(let* ((fortunes
        '("You will become a great Lisp Programmer"
          "The force will not be with you"
          "Take time for meditation"))
       (len (length fortunes))
       (index 0))
  (defun fortune ()
    (let ((new-fortune (nth index fortunes)))
      (setq index (1+ index))
      (if (>= index len) (setq index 0))
      new-fortune)))
~~~~~~~~

Here the function **fortune** is defined inside a **let** form. Because the local variable **fortunes** is referenced inside the function **fortune**, the variable **fortunes** exists after the **let** form is evaluated. It is important to understand that usually a local variable defined inside a **let** form "goes out of scope" and can no longer be referenced after the **let** form is evaluated.

However, in this example, there is no way to access the contents of the variable **fortunes** except by calling the function **fortune**. At a minimum, closures are a great way to hide variables. Here is some output from loading the **src/closure1.lisp** file and calling the function fortune several times:

~~~~~~~~
* (load "closure1.lisp")
;; Loading file closure1.lisp ...
;; Loading of file closure1.lisp is finished.
T
* (fortune)
"You will become a great Lisp Programmer"
* (fortune)
"The force will not be with you"
* (fortune)
"Take time for meditation"
* (fortune)
"You will become a great Lisp Programmer"
* 
~~~~~~~~

## Using the Function eval

In Lisp languages we often say that code is data. The function **eval** can be used to execute code that is stored as Lisp data. Let's look at an example:

{lang="lisp",linenos=on}
~~~~~~~~
$ ccl
Clozure Common Lisp Version 1.12  DarwinX8664
? '(+ 1 2.2)
(+ 1 2.2)
? (eval '(+ 1 2.2))
3.2
? (eval '(defun foo2 (x) (+ x x)))
FOO2
? (foo2 4)
8
~~~~~~~~

I leave it up to you, dear reader, how often you are motivated to use **eval**. In forty years of using Lisp languages my principle use of **eval** has been in modifying the standard version of the [Ops5 programming language for production systems](https://github.com/sharplispers/ops5) to support things like multiple data worlds and new actions to spawn off new data worlds and to remove them. Ops5 works by finding common expressions in a set of production rules (also referred to as "expert systems") and factoring them into a network (a Rete network if you want to look it up) with common expressions in rules stored in just a single place. **eval** is used a lot in Ops5 and I used it for my extensions to Ops5.

