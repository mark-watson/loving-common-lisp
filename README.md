# Loving Common Lisp, or the Savvy Programmer's Secret Weapon, 9th Edition

If you would like to support my work please consider purchasing my books on [Leanpub](https://leanpub.com/u/markwatson) and star my git repositories that you find useful on [GitHub](https://github.com/mark-watson?tab=repositories&q=&type=public). You can also interact with me on social media on [Mastodon](https://mastodon.social/@mark_watson) and [Twitter](https://twitter.com/mark_l_watson).

This repo is for the 9th edition of my book that will be released August 2025.

Open source examples in this repository are examples in my book "Loving Common Lisp, or the Savvy Programmer's Secret Weapon" that is available at https://leanpub.com/lovinglisp My book is available to read free online and the PDF for the book is licensed with Creative Common share with no modifications or commercial reuse: this means that you are encouraged to share copies (up to date copies only, please) of the PDF with friends and co-workers. New editions of the book are free if you have purchased the book before. Many thanks to the people who support my writing by purchasing the book!

The source code in this repository may be used either under the Apache 2 or the LGPL version 3 licenses. Use whichever license that works best for you.

Starting December 5, 2020 I have moved most of my personal (non-work related) Common Lisp projects to this repo for my book examples. You may notice subdirectories in **src** that contain code that is not yet (or may never be) in my book. These directories contain the file **NOT_YET_IN_BOOK.md**.

If you notice any errors or have suggestions for improvement, then git pull requests will be appreciated!


## Configure Quicklisp to find my book examples

Let's start with configuriong SBCL and Quicklisp:

Edit your file **~/.sbclrc** to add the following line of code:

(pushnew #p"/Users/mark/GITHUB/loving-common-lisp/"
         ql:*local-project-directories*
         :test #'equal)

**NOTE:** Please change the path #p"/Users/mark/GITHUB/loving-common-lisp/" to the path where you cloned this repository using:

    git clone https://github.com/mark-watson/loving-common-lisp.git


## Please visit my web site

My website can be found here: [https://markwatson.com](https://markwatson.com)
