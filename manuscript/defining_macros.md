# Defining Common Lisp Macros

We saw in the last chapter how the Lisp function **eval** could be used to evaluate arbitrary Lisp code stored in lists. Because **eval** is inefficient, a better way to generate Lisp code automatically is to define macro expressions that are expanded inline when they are used. In most Common Lisp systems, using **eval** requires the Lisp compiler to compile a form on-the-fly which is not very efficient. Some Lisp implementations use an interpreter for eval which is likely to be faster but might lead to obscure bugs if the interpreter and compiled code do not function identically.

The ability to add functionality and syntax to the Common Lisp language, to in effect extend the language as needed, is truly a super power of languages like Common Lisp and Scheme.

## Example Macro

The file **src/macro1.lisp** contains both a simple macro and a function that uses the macro. This macro example is a bit contrived since it could be just a function definition, but it does show the process of creating and using a macro. We are using the **gensym** function to define a new unique symbol to reference a temporary variable:

{lang="lisp",linenos=on}
~~~~~~~~
;; first simple macro example:

(defmacro double-list (a-list)
  (let ((ret (gensym)))
    `(let ((,ret nil))
       (dolist (x ,a-list)
         (setq ,ret (append ,ret (list x x))))
       ,ret)))

;; use the macro:

(defun test (x)
  (double-list x))
~~~~~~~~

The backquote character seen at the beginning of line 5 is used to quote a list in a special way: nothing in the list is evaluated during macro expansion unless it is immediately preceded by a comma character. In this case, we specify **,a-list** because we want the value of the macro's argument a-list to be substituted into the specially quoted list. We will look at **dolist** in some detail in the next chapter but for now it is sufficient to understand that **dolist** is used to iterate through the top-level elements of a list, for example:

~~~~~~~~
* (dolist (x '("the" "cat" "bit" "the" "rat"))
       (print x))
"the" 
"cat" 
"bit" 
"the" 
"rat" 
NIL
* 
~~~~~~~~

Notice that the  example macro **double-list** itself uses the macro **dolist**. It is common to nest macros in the same way functions can be nested.

Returning to our macro example in the file **src/macro1.lisp**, we will try the function **test** that uses the macro **double-list**:

~~~~~~~~
* (load "macro1.lisp")
;; Loading file macro1.lisp ...
;; Loading of file macro1.lisp is finished.
T
* (test '(1 2 3))
(1 1 2 2 3 3)
*
~~~~~~~~

## Using the Splicing Operator

Another similar example is in the file **src/macro2.lisp**:

{lang="lisp",linenos=on}
~~~~~~~~
;; another macro example that uses ,@:

(defmacro double-args (&rest args)
  `(let ((ret nil))
    (dolist (x ,@args)
      (setq ret (append ret (list x x))))
    ret))

;; use the macro:

(defun test (&rest x)
  (double-args x))
~~~~~~~~

Here, the splicing operator ,@ is used to substitute in the list args in the macro double-args.

## Using macroexpand-1

The function **macroexpand-1** is used to transform macros with arguments into new Lisp expressions. For example:

~~~~~~~~
* (defmacro double (a-number)                       
        (list '+ a-number a-number))
DOUBLE
* (macroexpand-1 '(double n))
(+ N N) ;
T
* 
~~~~~~~~

Writing macros is an effective way to extend the Lisp language because you can control the code passed to the Common Lisp compiler. In both macro example files, when the function **test** was defined, the macro expansion is done before the compiler processes the code. We will see in the next chapter several useful macros included in Common Lisp.

We have only "scratched the surface" looking at macros; the interested reader is encouraged to search the web using, for example, "Common Lisp macros." There are two books in particular that I recommend that take a deep dive into Common Lisp macros: Paul Graham's "On Lisp" and Doug Hoyte's "Let Over Lambda." Both are deep books and will change the way you experience software development. A good plan of study is spending a year absorbing "On Lisp" before tackling "Let Over Lambda."
