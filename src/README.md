# My Common Lisp Playground

## notes for new projects from Xach:

https://xach.livejournal.com/278047.html

Store root for my quicklisp libraries in:

        ~/.config/common-lisp/source-registry.conf.d/projects.conf

that currently contains:

````````
(:tree
 (:home "GITHUB/common-lisp/")
)

NOTE for Ubuntu Linux: this did not have to work. Instead I did a ln -s mapping to ~/.local/share/common-lisp/source/

````````

Note, for easier debugging, but NOT for shipping product, in .sbclrc:

;; slows code but makes debugging simpler:
(declaim (optimize (debug 3)))

(setf *print-case* :downcase)



# Use for Common Lisp component of KGcreator

some good references:  auto-build knowledge graphs


