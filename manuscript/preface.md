# Cover Material, Copyright, and License

Copyright 2011-2025 Mark Watson. All rights reserved. This book may be shared using the Creative Commons "share and share alike, no modifications, no commercial reuse" license.

This eBook will be updated occasionally so please periodically check the [leanpub.com web page for this book](https://leanpub.com/lovinglisp) for updates.

Please visit the [author's website](http://markwatson.com).

If you found a copy of this book on the web and find it of value then please consider buying a copy at [leanpub.com/lovinglisp](https://leanpub.com/lovinglisp).

If you would like to support my work please consider purchasing my books on [Leanpub](https://leanpub.com/u/markwatson) and star my git repositories that you find useful on [GitHub](https://github.com/mark-watson?tab=repositories&q=&type=public). You can also interact with me on social media on [Mastodon](https://mastodon.social/@mark_watson) and [Twitter](https://twitter.com/mark_l_watson).

# Preface

This book has been a continual work in progress since 2011. This current edition is released on January 28, 2025. I have used Common Lisp professionally since 1982 and I love the language!

I hope you have fun with this book and that you find the content useful for your own projects.

## How To Read This Book?

I have been using Common Lisp professionally since 1982 and also use Common Lisp for my own experiments. This book contains many little ideas and many little bits of code. If you already know Common Lisp then I recommend looking at the table of contents and simply starting with the topics that interest you the most.

## Requests from the Author

This book will always be available to read free online at [https://leanpub.com/lovinglisp/read](https://leanpub.com/lovinglisp/read).

That said, I appreciate it when readers purchase my books because the income enables me to spend more time writing.

### Hire the Author as a Consultant

I am available for short consulting projects. Please see [https://markwatson.com](https://markwatson.com).


## Why Use Common Lisp?

Why Common Lisp? Isn't Common Lisp an old language? Do many people still use Common Lisp?

I believe that using Lisp languages like Common Lisp, Clojure, Racket, and Scheme are all secret weapons useful in agile software development. An interactive development process and live production updates feel like a breath of fresh air if you have development on *heavy* *weight* like Java Enterprise Edition (JEE).

Yes, Common Lisp is an old language but with age comes stability and extremely good compiler technology. There is also a little inconsistency between different Common Lisp systems in such things as handling threads but with a little up front knowledge you can choose which Common Lisp systems will support your requirements.

 
## Acknowledgments

I would like to thank [Jans Aasman](https://en.wikipedia.org/wiki/Jans_Aasman) for contributing as technical editor for the fourth edition of this book. Jans is CEO of [Franz.com](http://franz.com/) which sells [Allegro Common Lisp](http://franz.com/products/allegro-common-lisp/) as well as tools for semantic web and linked data applications.

I would like to thank the following people who made suggestions for improving previous editions of this book:

Sam Steingold, Andrew Philpot, Kenny Tilton, Mathew Villeneuve, Eli Draluk, Erik Winkels, Adam Shimali, and Paolo Amoroso.

I would like to also thank several people who pointed out typo errors in this book and for specific suggestions:  Martin Lightheart, Tong-Kiat Tan, Rainer Joswig, Gerold Rupprecht, HN member rurban, David Cortesi. I would like to thank the following Reddit /r/lisp readers who pointed out mistakes in the fifth edition of this book: arnulfslayer, rpiirp, and itmuckel. I would like to thank Ted Briscoe for pointing out a problem with the spacy web client example in the 6th edition.

I would like to thank Paul Graham for coining the phrase "The Secret Weapon" (in his excellent paper "Beating the Averages") in discussing the advantages of Lisp and giving me permission to reuse his phrase.

#### I would especially like to thank my wife Carol Watson for her fine work in editing this book.

## Setting Up Your Common Lisp Development System and Quicklisp

These instructions assume the use of SBCL. See comments for LispWorks, Franz Common Lisp, and Closure Common List at the end of this section. I assume that you have installed SBCL and Quicklisp by following the instructions at [lisp-lang.org/learn/getting-started](https://lisp-lang.org/learn/getting-started/). These instructions also guide you through installing the Slime extensions for Emacs. I use both Emacs + Slime and VSCode with Common Lisp plugins for editing Common Lisp. If you like VSCode then I recommend Yasuhiro Matsumoto's Lisp plugin for syntax highlighting. For both Emacs and VSCode I usually run a separate REPL in a terminal window and don't run an editor-integrated REPL. I think that I am in the minority in using a separate REPL running in a shell.

I have been using Common Lisp since about 1982 and Quicklisp (developed and maintained by [Zach Beane](https://www.xach.com)) has been the most revolutionary change in my Common Lisp development (even more so than getting a hardware Lisp Machine and the availability of Coral Common Lisp on the Macintosh).

You can follow the directions on the main GitHub repository for this book: [https://github.com/mark-watson/loving-common-lisp](https://github.com/mark-watson/loving-common-lisp) to get the examples set up to run on your computer. Starting with the 8th edition, I have a new scheme for distributing the book examples on GitHub:

- A few short example Common Lisp code snippets are still kept in the main repository for the book: [https://github.com/mark-watson/loving-common-lisp](https://github.com/mark-watson/loving-common-lisp).
- The longer examples are now stored in separate GitHub repositories to facilitate using them as reusable Quicklisp libraries.
- Clone the main GitHub repository and copy the [Makefile](https://raw.githubusercontent.com/mark-watson/loving-common-lisp/master/Makefile) to the directory **~/quicklisp/local-projects/** on your computer.
- Change directory to **~/quicklisp/local-projects/** and run the **Makefile** target **make fetch** to copy all separate GitHub repositories to subdirectories of **~/quicklisp/local-projects/**.
- You can now load any book example using Quicklisp, for example: **(ql:quickload :sparql)**.


For example, the subdirectory loving-common-lisp/src/spacy-py4cl contains a package named **spacy-py4cl** that can now be accessed from any directory on your system using:

~~~~~~~~
$ sbcl
(ql:quickload "spacy-py4cl")
* (spacy-py4cl:nlp "My sister has a dog Henry. She loves him.")
* (defvar x (spacy-py4cl:nlp "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico."))
~~~~~~~~

This example uses the deep learning NLP models in spaCy which is a Python library - see the chapter on NLP for details on installing the Python dependencies. Note that only a few examples in this book require Python dependencies.

I have used the SBCL implementation of Common Lisp in this book. There are many fine Common Lisp implementations from Franz, LispWorks, Clozure Common Lisp, etc. I usually use LispWorks for my professional development work. If you have any great difficulty adopting the examples to your choice of Common Lisp implementations and performing web search does not suggest a solution then you can reach me through my web site, [markwatson.com](https://markwatson.com).
