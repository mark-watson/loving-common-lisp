# Using the PY4CL Library to Embed Python in Common Lisp

We will tackle the same problem as the previous chapter but take a different approach. Now we will use Ben Dudson's project [Py4CL](https://github.com/bendudson/py4cl/) that automatically starts a Python process and communicates with the Python process via a stream interface. The approach we took before is appropriate for large scale systems where you might want scale horizontally by having Python processes running on different servers than the servers used for the Common Lisp parts of your application. The approach we now take is much more convenient for what I call "laptop development" where the management of a Python process and communication is handled for you by the Py4CL library. If you need to build multi-server distributed systems for scaling reasons then use the examples in the last chapter.

While Py4CL provides a lot of flexibility for passing primitive types between Common Lisp and Python (in both directions), I find it easiest to write small Python wrappers that only use lists, arrays, numbers, and strings as arguments and return types. You might want to experiment with the examples on the Py4CL GitHub page that let you directly call Python libraries without writing wrappers. When I write code for my own projects I try to make code as simple as possible so when I need to later revisit my own code it is immediately obvious what it is doing. Since I have been using Common Lisp for almost 40 years, I often find myself reusing bits of my own old code and I optimize for making this as easy as possible. In other words I favor readability over "clever" code.

## Project Structure, Building the Python Wrapper, and Running an Example

The packaging of the Lisp code for my **spacy-py4cl** package is simple. Here is the listing of **package.lisp** for this project:

{lang="lisp",linenos=on}
~~~~~~~~
;;;; package.lisp

(defpackage #:spacy-py4cl
  (:use #:cl #:py4cl)
  (:export #:nlp))
~~~~~~~~

Listing of **spacy-py4cl.asd**:

{lang="lisp",linenos=on}
~~~~~~~~
;;;; spacy-py4cl.asd

(asdf:defsystem #:spacy-py4cl
  :description "Use py4cl to use Python spaCy library embedded in Common Lisp"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  :depends-on (#:py4cl)
  :serial t
  :components ((:file "package")
               (:file "spacy-py4cl")))
~~~~~~~~

You need to run a Python setup procedure to install the Python wrapper for space-py4cl on your system. Some output is removed for conciseness:

{lang="bash",linenos=on}
~~~~~~~~
$ cd loving-common-lisp/src/spacy-py4cl
$ cd PYTHON_SPACY_SETUP_install/spacystub
$ pip install -U spacy
$ python -m spacy download en
$ python setup.py install
running install
running build
running build_py
running install_lib
running install_egg_info
Writing /Users/markw/bin/anaconda3/lib/python3.7/site-packages/spacystub-0.21-py3.7.egg-info
~~~~~~~~

You only need to do this once unless you update to a later version of Python on your system.

If you are not familiar with Python, it is worth looking at the wrapper implementation, otherwise skip the next few paragraphs. 

{lang="bash",linenos=off}
~~~~~~~~
$ ls -R PYTHON_SPACY_SETUP_install 
spacystub

PYTHON_SPACY_SETUP_install/spacystub:
README.md		setup.py	spacystub

PYTHON_SPACY_SETUP_install/spacystub/build/lib:
spacystub

PYTHON_SPACY_SETUP_install/spacystub/spacystub:
parse.py
~~~~~~~~

Here is the implementation of **setup.py** that specifies how to build and install the wrapper globally for use on your system:

{lang="python",linenos=on}
~~~~~~~~
from distutils.core import setup

setup(name='spacystub',
      version='0.21',
      packages=['spacystub'],
      license='Apache 2',
      py_modules=['pystub'],
      long_description=open('README.md').read())
~~~~~~~~

The definition of the library in file **PYTHON_SPACY_SETUP_install/spacystub/spacystub/parse.py**:

{lang="python",linenos=on}
~~~~~~~~
import spacy

nlp = spacy.load("en")

def parse(text):
  doc = nlp(text)
  response = {}
  response['entities'] = [(ent.text, ent.start_char, ent.end_char, ent.label_) for ent in doc.ents]
  response['tokens'] = [token.text for token in doc]
  return [response['tokens'], response['entities']]
~~~~~~~~

Here is a Common Lisp repl session showing you how to use the library implemented in the next section:

{lang="lisp",linenos=on}
~~~~~~~~
$ ccl 
Clozure Common Lisp Version 1.12  DarwinX8664

For more information about CCL, please see http://ccl.clozure.com.

CCL is free software.  It is distributed under the terms of the Apache Licence, Version 2.0.
? (ql:quickload "spacy-py4cl")
To load "spacy-py4cl":
  Load 1 ASDF system:
    spacy-py4cl
; Loading "spacy-py4cl"
[package spacy-py4cl]
("spacy-py4cl")
? (spacy-py4cl:nlp "The President of Mexico went to Canada")
#(#("The" "President" "of" "Mexico" "went" "to" "Canada") #(("Mexico" 17 23 "GPE") ("Canada" 32 38 "GPE")))
? (spacy-py4cl:nlp "Bill Clinton bought a red car. He drove it to the beach.")
#(#("Bill" "Clinton" "bought" "a" "red" "car" "." "He" "drove" "it" "to" "the" "beach" ".") #(("Bill Clinton" 0 12 "PERSON")))
~~~~~~~~

Entities in text are identified with the starting and ending character indices that refer to the input string. For example, the entity "Mexico" starts at character position 17 and character index 23 is the character after the entity name in the input string. The entity type "GPE" refers to a country name and "PERSON" refers to a person's name in the input text. 

## Implementation of spacy-py4cl

The Common Lisp implementation for this package is simple. In line 5 the call to **py4cl:python-exec** starts a process to run Python and imports the function **parse** from my Python wrapper. The call to **py4cl:import-function** in line 6 finds a function named "parse" in the attached Python process and generates a Common Lisp function with the same name that handles calling into Python and converting handling the returned values to Common Lisp values:

{lang="lisp",linenos=on}
~~~~~~~~
;;;; spacy-py4cl.lisp

(in-package #:spacy-py4cl)

(py4cl:python-exec "from spacystub.parse import parse")
(py4cl:import-function "parse")

(defun nlp (text)
  (parse text))
~~~~~~~~

While it is possible to call Python libraries directly using Py4CL, when I need to frequently use Python libraries like spaCY, TensorFlow, fast.ai, etc. in Common Lisp, I like to use wrappers that use simple as possible data types and APIs to communicate between a Common Lisp process and the spawned Python process.

## Trouble Shooting Possible Problems - Skip if this Example Works on Your System

When you install my wrapper library in Python on the command line whatever your shell if (bash, zsh, etc.) you should then try to import the library in a Python repl:

{lang="bash",linenos=on}
~~~~~~~~
$ python
Python 3.7.4 (default, Aug 13 2019, 15:17:50) 
[Clang 4.0.1 (tags/RELEASE_401/final)] :: Anaconda, Inc. on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> from spacystub.parse import parse
>>> parse("John Smith is a Democrat")
[['John', 'Smith', 'is', 'a', 'Democrat'], [('John Smith', 0, 10, 'PERSON'), ('Democrat', 16, 24, 'NORP')]]
>>>
~~~~~~~~

If this works and the Common Lisp library **spacy-py4cl** does not, then make sure you are starting your Common Lisp program or running a Common Lisp repl with the same Python installation (if you have Quicklisp installed, then you also have the package **uiop** installed):

{lang="bash",linenos=on}
~~~~~~~~
$ which python
/Users/markw/bin/anaconda3/bin/python
$ sbcl
This is SBCL 2.0.2, an implementation of ANSI Common Lisp.
* (uiop:run-program "which python" :output :string)
"/Users/markw/bin/anaconda3/bin/python"
nil
0
* 
~~~~~~~~

If you run Common Lisp in an IDE (for example in LispWorks' IDE or VSCode with a Common Lisp plugin) make sure you start the IDE from the command line so your PATH environment variable will be set as it is in our bash or zsh shell.

## Wrap-up for Using Py4CL

While I prefer Common Lisp for general development and also AI research, there are useful Python libraries that I want to integrate into my projects. I hope that the last chapter and this chapter provide you with two solid approaches for you to use in your own work to take advantage of Python libraries.

