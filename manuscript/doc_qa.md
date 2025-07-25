# Using a Local Document Embeddings Vector Database With OpenAI GPT3 APIs for Semantically Querying Your Own Data

This project is inspired by the Python LangChain and LlamaIndex projects, with just the parts I need for my projects written from scratch in Common Lisp. I wrote a Python book "LangChain and LlamaIndex Projects Lab Book: Hooking Large Language Models Up to the Real World Using GPT-3, ChatGPT, and Hugging Face Models in Applications" in March 2023: [https://leanpub.com/langchain](https://leanpub.com/langchain) that you might also be interested in.

The GitHub repository for this example can be found here: [https://github.com/mark-watson/docs-qa](https://github.com/mark-watson/docs-qa). This code also requires my OpenAI Common Lisp library [https://github.com/mark-watson/openai](https://github.com/mark-watson/openai).

## Overview of Local Embeddings Vector Database to Enhance the Use of GPT3 APIs With Local Documents

In this example we will use the SqLite database to store the text from documents as well as OpenAI embedding vectors for the text. Each embedding vector is 1536 floating point numbers. Two documents are semantically similar if the dot product of their embedding vectors is large.

For long documents, we extract the text and create multiple chunks of text. Each chunk is stored as a row in a SqLite database table. This is an easy way to implement a vector datastore. There are many open source and commercial vector datastores if you reach performance limits with the simple techniques we use here.

For each text chunk we call an OpenAI API to get an embedding vector. Later when we want to have a GPT enabled conversation or just semantically query our local documents, we take the user's query and call an OpenAI API to get an embedding vector for the query text. We then compute the vector dot product between the query embedding vector and each chunk embedding vector. We save the text of the chunks that are semantically similar to the query embedding vector and use this text as "context text" that we pass to an OpenAI Large Language Model (LLM) API along with the user's original query text.

What does this process really do? Normally when you query ChatGPT or similar LLMs, we are querying against knowledge gained from all the original model training text. This process can lead to so-called "model hallucinations" where the model "makes stuff up." The advantage to the using the Python libraries LangChain and LlamaIndex is that a LLM is effectively using all original training data but is also primed with hopefully relevant context text from your local documents that might be useful for answering the user's query. We will replicate a small amount of this functionality in Common Lisp.

At the end of this chapter we will extend our code for single queries with a conversational example. Our approach to this is simple: when we pass context text and a query, we also pass previous conversational queries from the user. I am still experimenting with the ideas in this chapter so please do occasionally look for updates to the GitHub repository [https://github.com/mark-watson/docs-qa](https://github.com/mark-watson/docs-qa) and updates to this book.


## Implementing a Local Vector Database for Document Embeddings

In the following listing of the file **docs-qa.lisp** we start in lines 6-31 with a few string utility functions we will need: **write-floats-to-string**, **read-file**, **concat-strings**, **truncate-string**, and **break-into-chunks**.

The function **break-into-chunks** is a work in progress. For now we simply cut long input texts into specific chunk lengths, often cutting words in half. A future improvement will be detecting sentence boundaries and breaking text on sentences. The Python libraries LangChain and LlamaIndex have multiple chunking strategies.

In lines 33-37 function **decode-row** takes data from a SQL query to fetch a database table row and extracts the original chunk text and the embedding vector. Because of the overhead of making many calls to the OpenAI APIs the time spent running the local Common Lisp example code is very small so I have not yet worked on making my code efficient.


```lisp
(ql:quickload :sqlite)
(use-package :sqlite)

;; define the environment variable "OPENAI_KEY" with the value of your OpenAI API key

(defun write-floats-to-string (lst)
  (with-output-to-string (out)
    (format out "( ")
    (loop for i in lst
          do (format out "~f " i))
    (format out " )")))

(defun read-file (infile) ;; from Bing+ChatGPT
  (with-open-file (instream infile
                            :direction :input
                            :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun concat-strings (list)
  (apply #'concatenate 'string list))

(defun truncate-string (string length)
  (subseq string 0 (min length (length string))))

(defun break-into-chunks (text chunk-size)
  "Breaks TEXT into chunks of size CHUNK-SIZE."
  (loop for start from 0 below (length text) by chunk-size
        collect (subseq text start (min (+ start chunk-size) (length text)))))

(defun decode-row (row)
  (let ((id (nth 0 row))
        (context (nth 1 row))
        (embedding (read-from-string (nth 2 row))))
    (list id context embedding)))
```

The next listing shows of parts of **docs-qa.lisp** that contain code to use SqLite. I wrapped the calls to initialize the database inside of **handler-case** for convenience during development (file reloads don't throw top level errors and the existing database is untouched).


```lisp
(defvar *db* (connect ":memory:"))
;;(defvar *db* (connect "test.db"))

(pprint *db*)
(handler-case
    (progn
      (execute-non-query
       *db*
       "CREATE TABLE documents (document_path TEXT, content TEXT, embedding TEXT);")
      (execute-non-query
       *db*
       "CREATE INDEX idx_documents_id ON documents (document_path);")
      (execute-non-query
       *db*
       "CREATE INDEX idx_documents_content ON documents (content);")
      (execute-non-query
       *db*
       "CREATE INDEX idx_documents_embedding ON documents (embedding);"))
 (error (c)
   (print "Database and indices is already created")))

(defun insert-document (document_path content embedding)
  ;;(format t "insert-document:~% content:~A~%  embedding: ~A~%" content embedding)
  (format t "~%insert-document:~%  content:~A~%~%" content)
  (execute-non-query
   *db*
   "INSERT INTO documents (document_path, content, embedding) VALUES (?, ?, ?);"
   document_path content (write-floats-to-string embedding)))

(defun get-document-by-document_path (document_path)
  (mapcar #'decode-row
            (execute-to-list *db*
                             "SELECT * FROM documents WHERE document_path = ?;"
                             document_path)))

(defun get-document-by-content (content)
  (mapcar #'decode-row 
    (execute-to-list *db*
                     "SELECT * FROM documents WHERE content LIKE ?;" content)))

(defun get-document-by-embedding (embedding)
 (mapcar #'decode-row 
   (execute-to-list *db*
                    "SELECT * FROM documents WHERE embedding LIKE ?;" embedding)))

(defun all-documents ()
  (mapcar #'decode-row 
    (execute-to-list *db* "SELECT * FROM documents;")))

(defun create-document (fpath)
  (let ((contents (break-into-chunks (read-file fpath) 200)))
    (dolist (content contents)
      (handler-case	  
	  (let ((embedding (openai::embeddings content)))
	    (insert-document fpath content embedding))
	(error (c)
	       (format t "Error: ~&~a~%" c))))))
```

## Using Local Embeddings Vector Database With OpenAI GPT APIs

The next listing showing of parts of **docs-qa.lisp** interfaces with the OpenAI APIs:

```lisp
(defun qa (question)
  (let ((answer (openai:answer-question question 60)))
    (format t "~&~a~%" answer)))

(defun semantic-match (query custom-context &optional (cutoff 0.7))
  (let ((emb (openai::embeddings query))
        (ret))
    (dolist (doc (all-documents))
      (let ((context (nth 1 doc)) ;; ignore fpath for now
	    (embedding (nth 2 doc)))
	(let ((score (openai::dot-product emb embedding)))
	  (when (> score cutoff)
	    (push context ret)))))
    (format t "~%semantic-search: ret=~A~%" ret)
    (let* ((context (join-strings " . " (reverse ret)))
           (query-with-context
             (join-strings
               " "
               (list context custom-context
                 "Question:" query))))
      (openai:answer-question query-with-context 40))))

(defun QA (query &optional (quiet nil))
  (let ((answer (semantic-match query "")))
    (if (not quiet)
        (format t "~%~%** query: ~A~%** answer: ~A~%~%" query answer))
    answer))
```


## Testing Local Embeddings Vector Database With OpenAI GPT APIs

In the next part of the listing of **docs-qa.lisp** we write a test function to create two documents. The two calls to **create-document** actually save text and embeddings for about 20 text chunks in the database.

```lisp
(defun test()
  "Test code for Semantic Document Search Using
   OpenAI GPT APIs and local vector database"
  (create-document "data/sports.txt")
  (create-document "data/chemistry.txt")
  (QA "What is the history of the science of chemistry?")
  (QA "What are the advantages of engainging in sports?"))
```

The output is (with a lot of debug printout not shown):

```console
$ sbcl
* (quicklisp:quickload :docs-qa)
To load "docs-qa":
  Load 1 ASDF system:
    docs-qa
; Loading "docs-qa"
..................................................
[package docs-qa]To load "sqlite":
  Load 1 ASDF system:
    sqlite
; Loading "sqlite"

#<sqlite-handle {7005CA3783}>
(:docs-qa)
* (in-package :docs-qa)
#<package "DOCS-QA">
* (test)

** query: What is the history of the science of chemistry?
** answer: The history of chemistry as a science began in the 6th century BC, when the Greek philosopher Leucippus and his student Democritus posited the existence of an endless number of worlds

** query: What are the advantages of engainging in sports?
** answer: The advantages of engaging in sports are:n1. It helps to develop the body and mind.n2. It helps to develop the character.n3. It helps to develop the personality.
```


## Adding Chat History

In the last part of the listing of **docs-qa.lisp** we experiment with supporting a conversation/chat of multiple semantic queries against our local documents.

```lisp
(defun CHAT ()
  (let ((messages '(""))
        (responses '("")))
    (loop
       (format t "~%Enter chat (STOP or empty line to stop) >> ")
       (let ((string (read-line))
             response)
         (cond ((or (string= string "STOP") (< (length string) 1)) (return))
               (t (let (prompt
                        custom-context)
                    (setf custom-context
                          (concatenate
                           'string
                           "PREVIOUS CHAT: "
                           (join-strings  " "
                                          (reverse messages))))
                    (push string messages)
                    (print messages) ;; (print responses)
                    (print prompt)
                    (setf response (semantic-match string custom-context))
                    (push response responses)
                    (format t "~%Response: ~A~%" response))))))
    (list (reverse messages) (reverse responses))))
```

The output (with lots of debug printouts removed) looks like:

```console
$ sbcl
* (quicklisp:quickload :docs-qa)
To load "docs-qa":
  Load 1 ASDF system:
    docs-qa
; Loading "docs-qa"
..................................................
[package docs-qa].To load "sqlite":
  Load 1 ASDF system:
    sqlite
; Loading "sqlite"
#<sqlite-handle {7005D9B9D3}>
* (in-package :docs-qa)
#<package "DOCS-QA">
* (create-document "data/chemistry.txt")

insert-document:
  content:Amyl alcohol is an organic compound with the formula C 5 H 12 O. All eight isomers of amyl alcohol are known. The most important is isobutyl carbinol, this being the chief constituent of fermentation 
 ;; output from all other document chunks is not shown
 
 * (CHAT)

Enter chat (STOP or empty line to stop) >> what is the history of chemistry?

Response: Chemistry is the science of matter, its composition, structure and its properties. Chemistry is concerned with atoms and their interactions with other atoms, and thus is central to all other sciences. Chemistry is also concerned

Enter chat (STOP or empty line to stop) >> what is the boiling temperature?

Response: The boiling temperature of a liquid is the temperature at which the vapor pressure of the liquid equals the pressure surrounding the liquid, and the liquid changes into a vapor. At the boiling temperature, bubbles of vapor

Enter chat (STOP or empty line to stop) >> 
```

## Wrap Up for Using Local Embeddings Vector Database to Enhance the Use of GPT3 APIs With Local Documents

As I write this in early April 2023, I have been working almost exclusively with OpenAI APIs for the last year and using the Python libraries for LangChain and LlamaIndex for the last three months.

I prefer using Common Lisp over Python when I can, so I am implementing a tiny subset of the LangChain and LlamaIndex libraries in Common Lisp for my own use. By writing about my Common Lisp experiments here I hope that I get pull requests for [https://github.com/mark-watson/docs-qa](https://github.com/mark-watson/docs-qa) from readers who are interested in helping to extend the Common Lisp library.

