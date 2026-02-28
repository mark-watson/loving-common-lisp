# Common Lisp Package System  {#package_system}

In later chapters we will see two complete applications that are defined as Quicklisp projects: the [chapter on the Knowledge Graph Creator](#kgcreator) and the [chapter on the Knowledge Graph Navigator](#kgn). Another example for setting up a Quicklib project can be seen in the chapter [Plotting Data](#plotlib).

While these later chapters provide practical examples for bundling up your own projects in packages, the material here will give you general background information that you should know.

In the simple examples that we have seen so far, all newly created Lisp symbols have been placed in the default package. You can always check the current package by evaluating the expression *package*:

{linenos=off}
~~~~~~~~
> *package*
#<PACKAGE COMMON-LISP-USER>
> 
~~~~~~~~

As we will use in the following example, the package **:cl** is an alias for **:common-lisp-user**.

We will define a new package :my-new-package and two functions **foo1** and **foo2** inside the package. Externally to this package, assuming that it is loaded, we can access **foo2** using **my-new-package:foo2**. **foo1** is not exported so it cannot be accessed this way. However, we can always start a symbol name with a package name and two colon characters if we want to use a symbol defined in another package so we can use **my-new-package::foo1**. Using **::** allows us access to symbols not explicitly exported.

When I leave package **:my-new-package** in line 22 and return to package **:cl**, and try to access **my-new-package:foo1** notice that an error is thrown.

 
On line 3 we define the alias **:p1** for the package **:my-new-package** and we use this alias in line 44. The main point of the following example is that we define two functions in a package but only export one of these functions. By default the other function is not visible outside of the new package.

{linenos=on}
~~~~~~~~
* (defpackage "MY-NEW-PACKAGE"
    (:use :cl)
    (:nicknames "P1")
    (:export :FOO2))

#<PACKAGE "MY-NEW-PACKAGE">
* (in-package my-new-package)

#<PACKAGE "MY-NEW-PACKAGE">
* (defun foo1 () "foo1")

FOO1
* (defun foo2 () "foo2")

FOO2
* (foo1)

"foo1"
* (foo2)

"foo2"
* (in-package :cl)

#<PACKAGE "COMMON-LISP">
* (my-new-package:foo2)

"foo2"
* (my-new-package:foo1)

debugger invoked on a SB-INT:SIMPLE-READER-PACKAGE-ERROR in thread
#<THREAD "main thread" RUNNING {1001F1ECE3}>:
  The symbol "FOO1" is not external in the MY-NEW-PACKAGE package.

    Stream: #<SYNONYM-STREAM :SYMBOL SB-SYS:*STDIN* {100001C343}>

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE] Use symbol anyway.
  1: [ABORT   ] Exit debugger, returning to top level.

* 1
 
* (p1:foo2)

"foo2"
~~~~~~~~
     
Since we specified a nickname in the **defpackage** expression, Common Lisp allows the use of the nickname (in this case **P1**) in calling function **foo2** that is exported from package :my-new-package.

Near the end of the last example, we switched back to the default package COMMON-LISP-USER so we had to specify the package name for the function **foo2** on line 42.

What about the error on line 28 where **my-new-package:foo1** is undefined because the function **foo1** is not exported (see line 4)? It turns out that you can easily use symbols not exported from a package by using **::** instead of a single **:**. Here, this would be defined: **(my-new-package::foo1)**.

When you are writing very large Common Lisp programs, it is useful to be able to break up the program into different modules and place each module and all its required data in different name spaces by creating new packages. Remember that all symbols, including variables, generated symbols, CLOS methods, functions, and macros are in some package.

For small packages I sometimes put a **defpackage** expression at the top of the file immediately followed by an in-package expression to switch to the new package. In the general case, please properly use separate **project** and **asdf** files as I do in the later chapters [Knowledge Graph Creator](#kgcreator) and [Knowledge Graph Navigator](#kgn).
