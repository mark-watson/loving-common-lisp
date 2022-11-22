# Loving Common Lisp, or the Savvy Programmer's Secret Weapon, 6th Edition

If you would like to support my work please consider purchasing my books on [Leanpub](https://leanpub.com/u/markwatson) and star my git repositories that you find useful on [GitHub](https://github.com/mark-watson?tab=repositories&q=&type=public). You can also interact with me on social media on [Mastodon](https://mastodon.social/@mark_watson) and [Twitter](https://twitter.com/mark_l_watson).

This repo is for the 8th edition of my book that will be released August 2022. In the new edition of the book the examples are (mostly) moved to individual github projects. The **Makefile** in this repository is meant to be copied to your local
directory **~/quicklisp/local-projects** ad run the target **make fetch** to first get the self contained examples and later you can run the same make target to pull any available updates.

Open source examples for my book "Loving Common Lisp, or the Savvy Programmer's Secret Weapon" that is available at https://leanpub.com/lovinglisp My book is available to read free online and the PDF for the book is licensed with Creative Common share with no modifications or commercial reuse: this means that you are encouraged to share copies (up to date copies only, please) of the PDF with friends and co-workers. New editions of the book are free if you have purchased the book before. Many thanks to the people who support my writing by purchasing the book!

The source code in this repository may be used either under the Apache 2 or the LGPL version 3 licenses. Use whichever license that works best for you.

Starting December 5, 2020 I have moved most of my personal (non-work related) Common Lisp projects to this repo for my book examples. You may notice subdirectories in **src** that contain code that is not yet (or may never be) in my book. These directories contain the file **NOT_YET_IN_BOOK.md**.

If you notice any errors or have suggestions for improvement, then git pull requests will be appreciated!

## Separate GitHub repositories for this book

- conceptnet [https://github.com/mark-watson/conceptnet](https://github.com/mark-watson/conceptnet)
- kgn-common Knowledge Graph Navigator common library https://github.com/mark-watson/kgn-common
- kgn-text-ui Knowledge Graph Navigator text UI https://github.com/mark-watson/kgn-text-ui
- kgn-capi-ui Knowledge Graph Navigator LispWorks CAPI UI https://github.com/mark-watson/kgn-capi-ui

etc.

The **Makefile** can be copied to your local ~/quicklisp/local-projects, then **make fetch**.

## Changes for the new edition I am working on

May 2022: I am writing a new edition of my book, yay! I am going to move most (maybe all) of the examples in this book to individual GitHub repositories and as this happens I will put links to the new repositories right here! Yay, again.

In the current book I discuss setting the Quicklisp repository root directory to this Git repo. In the future, you will instead **git clone** operations in **~/quicklisp/local-projects** so, for example **(ql:quickload :sparql)** will work correctly. I hope that my readers find this convenient. In the future I want to write larger open source projects for new book material so it makes sense to use different GitHub repositories.

My website is https://markwatson.com
