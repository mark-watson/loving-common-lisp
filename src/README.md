# My Common Lisp Playground

## Most book examples are now in separate GitHub reositories.

The few that are not in separate repos are located here.

Please see the README.md file at the top directory of this repo for a description of the **Makefile** that can be used to fetch all book library examples to your local **~/quicklisp/local-projects/** directory.

Once copied to your local **~/quicklisp/local-projects/** directory then you can load any library using Quicklisp. For example:

    (ql:quickload :sparql-cache)
    (ql:quickload :openai)
    (ql:quickload :wordnet)
    (ql:quickload :fasttag)
    (ql:quickload :kbnlp)
    (ql:quickload :entities_dbpedia)
    (ql:quickload :bing)
    (ql:quickload :kgn-text-ui)
    (ql:quickload :kgsampler)
    (ql:quickload :spacy-py4cl) ; requires loading a Python library before use - see README.md file

etc.

As you read through my book, it should hopefully easy to load up and run the examples.

Some short examples are not in separate GitHub repositores - these examples are located here.


## Code experiments in this repository that are not (yet) in my Common Lisp book

If you see directories containing a file named NOT_YET_IN_BOOK.md then, as you might guess, that the directory contains something that I am still working on.


