# My Common Lisp Playground

## notes for new projects from Xach:

https://xach.livejournal.com/278047.html

I set the root for my quicklisp libraries in the file:

        ~/.config/common-lisp/source-registry.conf.d/projects.conf

that currently contains:

````````
(:tree
 (:home "GITHUB/loving-common-lisp/src/")
)

NOTE for Ubuntu Linux: this did not have to work. Instead I did a ln -s mapping to ~/.local/share/common-lisp/source/

````````

With this setup I can Quicklisp most of the projects in this **src** directory.


