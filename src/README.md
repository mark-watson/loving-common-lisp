# My Common Lisp Playground

## notes for new projects from Xach:

https://xach.livejournal.com/278047.html

## Making libraries (as defined in sub-directories of this directory) libraries that can be installed using Quicklisp

I set the root for my quicklisp libraries in the file:

        ~/.config/common-lisp/source-registry.conf.d/projects.conf

that currently contains:

````````
(:tree
 (:home "GITHUB/loving-common-lisp/src/")
)

NOTE for Ubuntu Linux: this did not have to work. Instead I did a ln -s mapping to ~/.local/share/common-lisp/source/

````````

With this setup I can Quicklisp quickload most of the projects in this **src** directory using (for example for directory **sparql**):

    (ql:quickload "sparql")

in any directory on my laptop.

## Code experiments in this repository that are not (yet) in my Common Lisp book

If you see directories containing a file named NOT_YET_IN_BOOK.md then, as you might guess, that the directory contains something that I am working on.


