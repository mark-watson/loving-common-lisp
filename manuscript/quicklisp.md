# Quicklisp  {#quicklisp}

For several decades managing packages and libraries was a manual process when developing Lisp systems. I used to package the source code for specific versions of libraries as part of my Common Lisp projects. Early package management systems mk-defsystem and ASDF were very useful, but I did not totally give up my practice keeping third party library source code with my projects until Zach Beane created the [Quicklisp package system](http://www.quicklisp.org/). You will need to have Quicklisp installed for many of the examples later in this book so please take the time to install it now as per the instructions on the Quicklisp web site.

## Using Quicklisp to Find Packages

We will need the Common Lisp Hunchentoot library later in the [Chapter on Network Programming](#network_prog) so we will install it now using Quicklisp as an example for getting started with Quicklisp.

We already know the package name we want, but as an example of discovering packages let's start by using Quicklisp to search for all packages with "hunchentoot" in the package name:

~~~~~~~~
* (ql:system-apropos "hunchentoot")
#<SYSTEM clack-handler-hunchentoot / clack-20131111-git / quicklisp 2013-11-11>
#<SYSTEM hunchentoot / hunchentoot-1.2.21 / quicklisp 2013-11-11>
#<SYSTEM hunchentoot-auth / hunchentoot-auth-20101107-git / quicklisp 2013-11-11>
#<SYSTEM hunchentoot-cgi / hunchentoot-cgi-20121125-git / quicklisp 2013-11-11>
#<SYSTEM hunchentoot-dev / hunchentoot-1.2.21 / quicklisp 2013-11-11>
#<SYSTEM hunchentoot-single-signon / hunchentoot-single-signon-20131111-git / quicklisp 2013-11-11>
#<SYSTEM hunchentoot-test / hunchentoot-1.2.21 / quicklisp 2013-11-11>
#<SYSTEM hunchentoot-vhost / hunchentoot-vhost-20110418-git / quicklisp 2013-11-11>
~~~~~~~~

We want the base package seen in line 3 and we can install the base package as seen in the following example:

~~~~~~~~
* (ql:quickload :hunchentoot)
To load "hunchentoot":
  Load 1 ASDF system:
    hunchentoot
; Loading "hunchentoot"
.......
(:HUNCHENTOOT)
~~~~~~~~

In line 1, I refer to the package name using a symbol :hunchentoot but using the string "hunchentoot" would have worked the same. The first time you **ql:quickload** a library you may see additional printout and it takes longer to load because the source code is downloaded from the web and cached locally in the directory **~/quicklisp/local-projects**. In most of the rest of this book, when I install or use a package by calling the **ql:quickload** function I do not show the output from this function in the repl listings.

Now, we can use the fantastically useful Common Lisp function **apropos** to see what was just installed:

~~~~~~~~
* (apropos "hunchentoot")

HUNCHENTOOT::*CLOSE-HUNCHENTOOT-STREAM* (bound)
HUNCHENTOOT:*HUNCHENTOOT-DEFAULT-EXTERNAL-FORMAT* (bound)
HUNCHENTOOT::*HUNCHENTOOT-STREAM*
HUNCHENTOOT:*HUNCHENTOOT-VERSION* (bound)
HUNCHENTOOT:HUNCHENTOOT-CONDITION
HUNCHENTOOT:HUNCHENTOOT-ERROR (fbound)
HUNCHENTOOT::HUNCHENTOOT-OPERATION-NOT-IMPLEMENTED-OPERATION (fbound)
HUNCHENTOOT::HUNCHENTOOT-SIMPLE-ERROR
HUNCHENTOOT::HUNCHENTOOT-SIMPLE-WARNING
HUNCHENTOOT::HUNCHENTOOT-WARN (fbound)
HUNCHENTOOT:HUNCHENTOOT-WARNING
HUNCHENTOOT-ASD:*HUNCHENTOOT-VERSION* (bound)
HUNCHENTOOT-ASD::HUNCHENTOOT
:HUNCHENTOOT (bound)
:HUNCHENTOOT-ASD (bound)
:HUNCHENTOOT-DEV (bound)
:HUNCHENTOOT-NO-SSL (bound)
:HUNCHENTOOT-TEST (bound)
:HUNCHENTOOT-VERSION (bound)
* 
~~~~~~~~

As long as you are thinking about the new tool Quicklisp that is now in your tool chest, you should install most of the packages and libraries that you will need for working through the rest of this book. I will show the statements needed to load more libraries without showing the output printed in the repl as each package is loaded:

~~~~~~~~
(ql:quickload "clsql")
(ql:quickload "clsql-postgresql")
(ql:quickload "clsql-mysql")
(ql:quickload "clsql-sqlite3")
(ql:quickload :drakma)
(ql:quickload :hunchentoot)
(ql:quickload :cl-json)
(ql:quickload "clouchdb")  ;; for CouchDB access
(ql:quickload "sqlite")
~~~~~~~~

You need to have the Postgres and MySQL client developer libraries installed on your system for the **clsql-postgresql** and **clsql-mysql** installations to work. If you are unlikely to use relational databases with Common Lisp then you might skip the effort of installing Postgres and MySQL. The example in the [Chapter on the Knowledge Graph Navigator](#kgn) uses the SQLite database for caching. You don't need any extra dependencies for the **sqlite** package.


## Using Quicklisp to Configure Emacs and Slime

I assume that you have Emacs installed on your system. In a repl you can setup the Slime package that allows Emacs to connect to a running Lisp environment:

{lang="lisp",linenos=off}
~~~~~~~~
(ql:quickload "quicklisp-slime-helper")
~~~~~~~~

Pay attention to the output in the repl. On my system the output contained the following:

~~~~~~~~
[package quicklisp-slime-helper]
slime-helper.el installed in "/Users/markw/quicklisp/slime-helper.el"

To use, add this to your ~/.emacs:

  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")
~~~~~~~~

If you installed **rlwrap** and defined an alias for running SBCL, make sure you set the inferior lisp program to the absolute path of the SBCL executable; on my system I set the following in my **.emacs** file:

~~~~~~~~
  (setq inferior-lisp-program "/Users/markw/sbcl/sbcl")
~~~~~~~~

I am not going to cover using Emacs and Slime, there are many good tutorials on the web you can read.

In later chapters we will write libraries and applications as Quicklisp projects so that you will be able to load your own libraries, making it easier to write small libraries that you can compose into larger applications.


