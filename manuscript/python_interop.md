# Using Python Deep Learning Models In Common Lisp With a Web Services Interface

In older editions of this book I had an example of using the Java DeepLearning4J deep learning library using Armed Bear Common Lisp, implemented in Java. I no longer use hybrid Java and Common Lisp applications in my own work and I decided to remove this example and replace it with two projects that use simple Python web services that act as wrappers for state of the art deep learning models with Common Lisp clients in the subdirectories:

- **src/spacy_web_client**: use the spaCy deep learning models for general NLP. I sometimes use my own pure Common Lisp NLP libraries we saw in earlier chapters and sometimes I use a Common Lisp client calling deep learning libraries like spaCy and TensorFlow.
- **src/coref_web_client**: coreference or anaphora resolution is the act of replacing pronouns in text with the original nouns that they refer to. This has traditionally been a very difficult and only partially solved problem until recent advances in deep learning models like BERT.

Note: in the next chapter we will cover similar functionality but we will use the **py4cl** library to more directly use Python and libraries like spaCy by starting another Python process and using streams for communication.

## Setting up the Python Web Services Used in this Chapter

You will need python and pip installed on your system. The source e code for the Python web services is found in the directory **loving-common-lisp/python**.

## Installing the spaCY NLP Services

I assume that you have some familiarity with using Python. If not, you will still be able to follow these directions assuming that you have the utilities **pip**, and **python** installed. I recommend installing Python and Pip using [Anaconda](https://anaconda.org/anaconda/conda).

The server code is in the subdirectory **python/python_spacy_nlp_server** where you will work when performing a one time initialization. After the server is installed you can then run it from the command line from any directory on your laptop.

I recommend that you use virtual Python environments when using Python applications to separate the dependencies required for each application or development project. Here I assume that you are running in a Python version 3.6 or higher environment. First you must install the dependencies:

~~~~~~~~
pip install -U spacy
python -m spacy download en
pip install falcon
~~~~~~~~

Then change directory to the subdirectory **python/python_spacy_nlp_server** in the git repo for this book and install the NLP server:

~~~~~~~~
cd python/python_spacy_nlp_server
python setup.py install
~~~~~~~~

Once you install the server, you can run it from any directory on your laptop or server using:

~~~~~~~~
spacynlpserver
~~~~~~~~

I use deep learning models written in Python using TensorFlow or PyTorch and provide Python web services  that can be used in applications I write in Haskell or Common Lisp using web client interfaces for the services written in Python. While it is possible to directly embed models in Haskell and Common Lisp, I find it much easier and developer friendly to wrap deep learning models I use a REST services as I have done here. Often deep learning models only require about a gigabyte of memory and using pre-trained models has lightweight CPU resource needs so while I am developing on my laptop I might have two or three models running and available as wrapped REST services. For production, I configure both the Python services and my Haskell and Common Lisp applications to start automatically on system startup.

This is not a Python programming book and I will not discuss the simple Python wrapping code but if you are also a Python developer you can easily read and understand the code.

## Installing the Coreference NLP Services


I recommend that you use virtual Python environments when using Python applications to separate the dependencies required for each application or development project. Here I assume that you are running in a Python version 3.6 environment. First you should install the dependencies:

~~~~~~~~
pip install spacy==2.1.0
pip install neuralcoref 
pip install falcon
~~~~~~~~

As I write this chapter the *neuralcoref* model and library require a slightly older version of SpaCy (the current latest version is 2.1.4).

Then change directory to the subdirectory **python/python_coreference_anaphora_resolution_server** in the git repo for this book and install the coref server:

~~~~~~~~
cd python_coreference_anaphora_resolution_server
python setup.py install
~~~~~~~~

Once you install the server, you can run it from any directory on your laptop or server using:

~~~~~~~~
corefserver
~~~~~~~~

While. as we saw in the last example, it is possible to directly embed models in Haskell and Common Lisp, I find it much easier and developer friendly to wrap deep learning models I use a REST services as I have done here. Often deep learning models only require about a gigabyte of memory and using pre-trained models has lightweight CPU resource needs so while I am developing on my laptop I might have two or three models running and available as wrapped REST services. For production, I configure both the Python services and my Haskell and Common Lisp applications to start automatically on system startup.

This is not a Python programming book and I will not discuss the simple Python wrapping code but if you are also a Python developer you can easily read and understand the code.


## Common Lisp Client for the spaCy NLP Web Services

Before looking at the code, I will show you typical output from running this example:

{lang="lisp",linenos=on}
~~~~~~~~
$ sbcl          
This is SBCL 1.3.16, an implementation of ANSI Common Lisp.
* (ql:quickload "spacy-web-client")
To load "spacy":
  Load 1 ASDF system:
    spacy-web-client
; Loading "spacy-web-client"
.........
("spacy-web-client")
* (defvar x
   (spacy-web-client:spacy-client
  "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico."))
* (spacy-web-client:spacy-data-entities x)
"Bill Clinton/PERSON"
* (spacy-web-client:spacy-data-tokens x)
("President" "Bill" "Clinton" "went" "to" "Congress" "." "He" "gave" "a"
 "speech" "on" "taxes" "and" "Mexico" ".")
~~~~~~~~
 
The client library is implemented in the file **src/spacy_web_client/spacy-web-client.lisp**:

{lang="lisp",linenos=on}
~~~~~~~~
(in-package spacy-web-client)

(defvar base-url "http://127.0.0.1:8008?text=")

(defstruct spacy-data entities tokens)
  
(defun spacy-client (query)
  (let* ((the-bytes
	  (drakma:http-request
	   (concatenate 'string
			base-url
			(do-urlencode:urlencode  query))
	   :content-type "application/text"))
	 (fetched-data
	  (flexi-streams:octets-to-string the-bytes :external-format :utf-8))
	 (lists (with-input-from-string (s fetched-data)
		  (json:decode-json s))))
    (print lists)
    (make-spacy-data :entities (cadar lists) :tokens (cdadr lists))))
~~~~~~~~

On line 3 we define base URL for accessing the spaCy web service, assuming that it is running on your laptop and not a remote server. On line 5 we define a **defstruct** named **spacy-data**  that has two fields: a list of entities in the input text and a list of word tokens in the input text.

The function **spacy-client** builds a query string on lines 10-12 that consists of the **base-url** and the input query text URL encoded. The drakma library, that we used before, is used to make a HTTP request from the Python spaCy server. Lines 14-15 uses the flexi-streams package to convert raw byte data to UTF8 characters. Lines 16-17 use the json package to parse the UTF8 encoded string, getting two lists of strings. I left the debug printout expression in line 18 so that you can see the results of parsing the JSON data. The function **make-spacy-data** was generated for us by the **defstruct** statement on line 5.

## Common Lisp Client for the Coreference NLP Web Services

Let's look at some typical output from this example, then we will look at the code:

{lang="lisp",linenos=on}
~~~~~~~~
$ sbcl
This is SBCL 1.3.16, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.

#P"/Users/markw/quicklisp/setup.lisp" 
"starting up quicklisp" 
* (ql:quickload "coref")
To load "coref":
  Load 1 ASDF system:
    coref
; Loading "coref"
..................................................
[package coref]
("coref")
* (coref:coref-client "My sister has a dog Henry. She loves him.")

"My sister has a dog Henry. My sister loves a dog Henry."
* (coref:coref-client "My sister has a dog Henry. He often runs to her.")

"My sister has a dog Henry. a dog Henry often runs to My sister."
~~~~~~~~

Notice that pronouns in the input text are correctly replaced by the noun phrases that the pronoun refer to.

The implementation for the core client is in the file **src/coref_web_client/coref.lisp**:

{lang="lisp",linenos=on}
~~~~~~~~
(in-package #:coref)

;; (ql:quickload :do-urlencode)

(defvar base-url "http://127.0.0.1:8000?text=")

(defun coref-client (query)
  (let ((the-bytes
	 (drakma:http-request
	  (concatenate 'string
		       base-url
		       (do-urlencode:urlencode  query)
		       "&no_detail=1")
	  :content-type "application/text")))
    (flexi-streams:octets-to-string the-bytes :external-format :utf-8)))
~~~~~~~~

This code is similar to the example in the last section for setting up a call to **http-request** but is simpler: here the Python coreference web service accepts a string as input and returns a string as output with pronouns replaced by the nouns or noun phrases that they refer to. The example in the last section had to parse returned JSON data, this example does not.

## Trouble Shooting Possible Problems - Skip if this Example Works on Your System

If you run Common Lisp in an IDE (for example in LispWorks' IDE or VSCode with a Common Lisp plugin) make sure you start the IDE from the command line so your PATH environment variable will be set as it is in our bash or zsh shell.

Make sure you are starting your Common Lisp program or running a Common Lisp repl with the same Python installation (if you have Quicklisp installed, then you also have the package **uiop** installed):

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


## Python Interop Wrap-up

Much of my professional work in the last five years involved deep learning models and currently most available software is written in Python. While there are available libraries for calling Python code from Common Lisp, these libraries tend to not work well for Python code using libraries like TensorFlow, spaCy, PyTorch, etc., especially if the Python code is configured to use GPUs via CUDA of special hardware like TPUs. I find it simpler to simply wrap functionality implemented in Python as a simple web service.