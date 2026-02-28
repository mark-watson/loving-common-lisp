# Introduction {#introduction}

This book is intended to get you, the reader, programming quickly in Common Lisp. Although the Lisp programming language is often associated with artificial intelligence, this introduction is on general Common Lisp programming techniques. Later we will look at general example applications and artificial intelligence examples.

The Common Lisp program examples are distributed [on the github repo for this book](https://github.com/mark-watson/loving-common-lisp).

## Why Did I Write this Book?

Why the title “Loving Common Lisp”? Simple! I have been using Lisp for almost 40 years and seldom do I find a better match between a programming language and the programming job at hand. I am not a total fanatic on Lisp, however. I often use Python for deep learning. I like Ruby, Java and Javascript for server side programming, and the few years that I spent working on Nintendo video games and virtual reality systems for SAIC and Disney, I found C++ to be a good bet because of stringent runtime performance requirements. For some jobs, I find the logic-programming paradigm useful: I also enjoy the Prolog language.

In any case, I love programming in Lisp, especially the industry standard Common Lisp. As I wrote the second edition of this book over a decade ago, I had been using Common Lisp almost exclusively for an artificial intelligence project for a health care company and for commercial product development. While working on the third edition of this book, I was not using Common Lisp professionally but since the release of the Quicklisp Common Lisp package manager I have found myself enjoying using Common Lisp more for small side projects. I use Quicklisp throughout in the third edition example code so you can easily install required libraries. For the fourth and fifth editions of this book I have added more examples using neural networks and deep learning. In this new sixth edition I have added a complete application that uses CAP for the user interface.

As programmers, we all (hopefully) enjoy applying our experience and brains for tackling interesting problems. My wife and I recently watched a two-night 7-hour PBS special “Joseph Campbell, and the Power of Myths.” Campbell, a college professor for almost 40 years, said that he always advised his students to “follow their bliss” and not to settle for jobs and avocations that are not what they truly want to do. That said I always feel that when a job calls for using Java, Python or other languages besides Lisp, that even though I may get a lot of pleasure from the job I am not following my bliss.

My goal in this book is to introduce you to one of my favorite programming languages, Common Lisp. I assume that you already know how to program in another language but if you are a complete beginner you can still master the material in this book with some effort. I challenge you to make this effort.


## Free Software Tools for Common Lisp Programming

There are several Common Lisp compilers and runtime tools available for free on the web:

- CLISP -- licensed under the GNU GPL and is available for Windows, Macintosh, and Linux/Unix
- Clozure Common Lisp (CCL) -- open source with good Mac OS X and Linux support
- CMU Common Lisp -- open source implementation
- SBCL -- derived from CMU Common Lisp
- ECL -- compiles using a separate C/C++ compiler
- ABCL -- Armed Bear Common Lisp for the JVM

There are also fine commercial Common Lisp products:

- LispWorks -- high quality and reasonably priced system for Windows and Linux. No charge for distributing compiled applications [lispworks.com](http://www.lispworks.com)
- Allegro Common Lisp - high quality, great support and higher cost. [franz.com](http://franz.com)
- MCL -- Macintosh Common Lisp. I used this Lisp environment in the late 1980s. MCL was so good that I gave away my Xerox 1108 Lisp Machine and switched to a Mac and MCL for my development work. Now open source but only runs on the old MacOS

I currently (mostly) use SBCL, CCL, and LispWorks. The SBCL compiler produces very fast code and the compiler warning can be of great value in finding potential problems with your code. Like CCL because it compiles quickly so is often preferable for development.

For working through this book, I will assume that you are using SBCL or CCL. For the example in the last chapter you will need LispWorks and the free Personal edition is fine for the purposes of experimenting with the example application and the CAPI user interface library.

## Making Book Examples Run Portably on Most Common Lisp Implementations

Many of the book examples require making web service calls. In general when I am writing Common Lisp applications that require making REST calls I prefer using 3rd party Common Lisp libraries like Drakma or Dexador. However it is sometimes a little tricky to set up Common Lisp on different operating systems and CPU architectures with **libopenssl**, **libcrypto**, etc. Because of this, in book examples I run the external **curl** program using **uiop:run-program** and collect the output as a string that is then parsed as JSON or CSV data. The overhead of starting an external process is very small compared to calling a web service so in your own applications you can either follow my example of using **curl** or use the Drakma or Dexador libraries. Using the Apple M1 processor on macOS can be particularly problematic with OpenSSL issues.

I also use the excellent Common Lisp to Python bridge library **py4cl** in a few book examples. Usually **py4cl** installs without problems. 

## How is Lisp Different from Languages like Java and C++?

This is a trick question! Lisp is slightly more similar to Java than C++ because of automated memory management so we will start by comparing Lisp and Java.

In Java, variables are strongly typed while in Common Lisp values are strongly typed. For example, consider the Java code:

~~~~~~~~
  Float x = new Float(3.14f);
  String s = "the cat ran" ;
  Object any_object = null;
  any_object = s;
  x = s;  // illegal: generates a
          // compilation error
~~~~~~~~
 
Here, in Java, variables are strongly typed so a variable x of type Float can't legally be assigned a string value: the code in line 5 would generate a compilation error. Lisp code can assign a value to a variable and then reassign another value of a different type.

Java and Lisp both provide automatic memory management. In either language, you can create new data structures and not worry about freeing memory when the data is no longer used, or to be more precise, is no longer referenced.

Common Lisp is an ANSI standard language. Portability between different Common Lisp implementations and on different platforms is very good. I have used Clozure Common Lisp, SBCL, Allegro Lisp (from Franz Inc), LispWorks, and CLISP that all run well on Windows, Mac OS X, and Linux. As a Common Lisp developer you will have great flexibility in tools and platforms.

ANSI Common Lisp was the first object oriented language to become an ANSI standard language. The Common Lisp Object System (CLOS) is probably the best platform for object oriented programming.

In C++ programs, a common bug that affects a program’s efficiency is forgetting to free memory that is no longer used. In a virtual memory system, the effect of a program’s increasing memory usage is usually just poorer system performance but can lead to system crashes or failures if all available virtual memory is exhausted. A worse type of C++ error is to free memory and then try to use it. Can you say “program crash”? C programs suffer from the same types of memory related errors.

Since computer processing power is usually much less expensive than the costs of software development, it is almost always worth while to give up a few percent of runtime efficiency and let the programming environment of runtime libraries manage memory for you. Languages like Lisp, Ruby, Python, and Java are said to perform automatic garbage collection.

I have written six books on Java, and I have been quoted as saying that for me, programming in Java is about twice as efficient (in terms of my time) as programming in C++. I base this statement on approximately ten years of C++ experience on projects for SAIC, PacBell, Angel Studios, Nintendo, and Disney. I find Common Lisp and other Lisp languages like Clojure and Scheme to be about twice as efficient (again, in terms of my time) as Java. That is correct: I am claiming a four times increase in my programming productivity when using Common Lisp vs. C++.

What do I mean by programming productivity? Simple: for a given job, how long does it take me to design, code, debug, and later maintain the software for a given task.

## Advantages of Working in a Lisp Environment

We will soon see that Lisp is not just a language; it is also a programming environment and runtime environment.

The beginning of this book introduces the basics of Lisp programming. In later chapters, we will develop interesting and non-trivial programs in Common Lisp that I argue would be more difficult to implement in other languages and programming environments.

The big win in programming in a Lisp environment is that you can set up an environment and interactively write new code and test new code in small pieces. We will cover programming with large amounts of data in the [Chapter on Natural Language Processing](#nlp_chapter), but let me share a a general use case for work that I do that is far more efficient in Lisp:

Much of my Lisp programming used to be writing commercial natural language processing (NLP) programs for my company www.knowledgebooks.com. My Lisp NLP code uses a large amount of memory resident data; for example: hash tables for different types of words, hash tables for text categorization, 200,000 proper nouns for place names (cities, counties, rivers, etc.), and about 40,000 common first and last names of various nationalities.

If I was writing my NLP products in C++, I would probably use a relational database to store this data because if I read all of this data into memory for each test run of a C++ program, I would wait 30 seconds every time that I ran a program test. When I start working in any Common Lisp environment, I do have to load the linguistic data into memory one time, but then can code/test/code/test... for hours with no startup overhead for reloading the data that my programs need to run. Because of the interactive nature of Lisp development, I can test small bits of code when tracking down errors and when writing new code.

It is a personal preference, but I find the combination of the stable Common Lisp language and an iterative Lisp programming environment to be much more productive than other languages and programming environments.
