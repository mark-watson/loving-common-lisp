# Using the OpenAI and Mistral APIs

I have been working as an artificial intelligence practitioner since 1982 and the capability of the beta OpenAI APIs is the most impressive thing that I have seen in my career so far. These APIs use the GPT-3 and GPT-4 models.

Note: in January 2024 I expanded this chapter to also include the hosted Mistral LLM APIs. I recently added material for locally hosted open weight LLM models like mistral-7b and lama2 to my [Racket Scheme AI](https://leanpub.com/racket-ai/read) (link to read online) book. I plan on adding that material to this book soon.

## History of OpenAI

OpenAI has been working on large language models (LLMs) since they were founded in December 2015. LLMs are artificial neural networks that can contain up to a trillion weights, and are trained using self-supervised learning and semi-supervised learning. OpenAI's LLMs can respond to written prompts with various types of content. The release of ChatGPT in November 2022 mainstreamed the idea that generative artificial intelligence could be used by companies and consumers to automate tasks, help with creative ideas, and write software in many different programming languages.


## Common Lisp Library for Using OpenAI APIs

I recommend reading the online documentation for the [online documentation for the APIs](https://beta.openai.com/docs/introduction/key-concepts) to see all the capabilities of the beta OpenAI APIs.  Let's start by jumping into the example code. As seen in the **package.lisp** file we use the **UIOP** and **cl-json** libraries and we export three top level functions:

{lang="lisp",linenos=on}
~~~~~~~~
;;;; package.lisp

(defpackage #:openai
  (:use #:cl #:uiop #:cl-json)
  (:export #:completions #:summarize #:answer-question))
~~~~~~~~


The library that I wrote for this chapter supports three functions that are exported from the package **openai**: for completing text, summarizing text, and answering general questions. The single OpenAI model that the beta OpenAI APIs use is fairly general purpose and can generate cooking directions when given an ingredient list, grammar correction, write an advertisement from a product description, generate spreadsheet data from data descriptions in English text, etc. 

Given the examples from [https://beta.openai.com](https://beta.openai.com) and the Common Lisp examples here, you should be able to modify my example code to use any of the functionality that OpenAI documents.

{lang="lisp",linenos=on}
~~~~~~~~
;;;; openai.asd

(asdf:defsystem #:openai
  :description "Library for using the beta OpenAI APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json)
  :components ((:file "package")
               (:file "openai")))
~~~~~~~~


We will look closely at the function **completions** and then just look at the small differences to the other two example functions. The definitions for all three exported functions are kept in the file **openai.lisp**. You need to request an API key (I had to wait a few weeks to receive my key) and set the value of the environment variable **OPENAI_KEY** to your key. You can add a statement like:

{linenos=off}
~~~~~~~~
export OPENAI_KEY=sa-hdffds7&dhdhsdgffd
~~~~~~~~

to your **.profile** or other shell resource file.

While I sometimes use pure Common Lisp libraries to make HTTP requests, I prefer running the **curl** utility as a separate process for these reasons:

- No problems with system specific dependencies.
- Use the standard library UIOP to run a shell command and capture the output as a string.
- I use **curl** from the command line when experimenting with web services. After I get working **curl** options, it is very easy to translate this into Common Lisp code.

An example **curl** command line call to the beta OpenAI APIs is:

{lang="bash",linenos=on}
~~~~~~~~
curl \
  https://api.openai.com/v1/engines/davinci/completions \
   -H "Content-Type: application/json"
   -H "Authorization: Bearer sa-hdffds7&dhdhsdgffd" \
   -d '{"prompt": "The President went to Congress", \
        "max_tokens": 22}'
~~~~~~~~

Here the API token "sa-hdffds7&dhdhsdgffd" on line 4 is made up - that is not my API token. All of the OpenAI APIs expect JSON data with query parameters. To use the completion API, we set values for **prompt** and **max_tokens**. The value of **max_tokens** is the requested number of returns words or tokens. We will look at several examples later.

Function call support was added to this library in April 2025. The following functions handle registering and using functions:

```lisp
;; Hash table to store available functions for tool calling
(defvar *available-functions* (make-hash-table :test 'equal))

(defstruct openai-function
  name
  description
  parameters
  func)

(defun register-function (name description parameters fn)
  (format t "Registering ~A ~A~%" name fn)
  (setf (gethash name *available-functions*)
        (make-openai-function
         :name name
         :description description
         :parameters parameters
	 :func fn)))
 
(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (subseq string (+ pos (length old))))
        string)))

(defun escape-json (str)
  (with-output-to-string (out)
    (loop for ch across str do
         (if (char= ch #\")
             (write-string "\\\"" out)
             (write-char ch out)))))


(defun handle-function-call (function-call)
  ;; function-call looks like: ((:name . "get_weather") (:arguments . "{\"location\":\"New York\"}"))
  (format t "~% ** handle-function-call (DUMMY) fucntion-call: ~A~%" function-call)
  (let* ((name (cdr (assoc :name function-call)))
         (args-string (cdr (assoc :arguments function-call)))
         (args (and args-string (cl-json:decode-json-from-string args-string)))
         (func (openai-function-func (gethash name *available-functions*))))
    (format t "~% handle-function-call name: ~A" name)
    (format t "~% handle-function-call args-string: ~A" args-string)
    (format t "~% handle-function-call args: ~A" args)
    (format t "~% handle-function-call func: ~A" func)
    (if (not (null func))
	(let ()
          (format t "~%Calling function ~a called with args: ~a~%" name args)
	  (let ((f-val (apply func (mapcar #'cdr args))))
	    (format t "~%Return value from func ~A is ~A~%" name f-val)
	    f-val))
        (error "Unknown function: ~a" name))))
```


In the file **openai.lisp** we define a helper function **openai-helper** that takes a string with the OpenAI API call arguments encoded as a **curl** command, calls the service, and then extracts the results from the returned JSON data:

{lang="lisp",linenos=on}
~~~~~~~~
(defun openai-helper (curl-command)
  ;;(terpri)
  ;;(princ curl-command)
  ;;(terpri)
  (let ((response (uiop:run-program curl-command
                                    :output :string
                                    :error-output :string)))
    (terpri)
    ;;(princ response)
    (terpri)
    (with-input-from-string (s response)
      (let* ((json-as-list (json:decode-json s))
             (choices (cdr (assoc :choices json-as-list)))
             (first-choice (car choices))
             (message (cdr (assoc :message first-choice)))
             (function-call (cdr (assoc :function--call message)))
             (content (cdr (assoc :content message))))
	;;(format t "~% json-as-list: ~A~%" json-as-list)
	;;(format t "~% choices: ~A~%" choices)
	;;(format t "~% first-choice: ~A~%" first-choice)
	;;(format t "~% message: ~A~%" message)
	;;(format t "~% function-call: ~A~%" function-call)
	;;(format t "~% content: ~A~%" content)
        (if function-call
            (handle-function-call function-call)
            (or content "No response content"))))))
~~~~~~~~

I convert JSON data to a Lisp list in line 12 and in line 14 I reach into the nested results list for the generated text string. You might want to add a debug printout statement to see the value of **json-as-list**.

The three example functions all use this **openai-helper** function. The first example function **completions** sets the parameters to complete a text fragment. You have probably seen examples of the OpenAI GPT models writing stories, given a starting sentence. We are using the functionality here:

{lang="lisp",linenos=on}
~~~~~~~~
(defun completions (starter-text &optional functions)
  (let* ((function-defs (when functions
                          (mapcar (lambda (f)
                                    (let ((func (gethash f *available-functions*)))
                                      (list (cons :name (openai-function-name func))
                                            (cons :description (openai-function-description func))
                                            (cons :parameters (openai-function-parameters func)))))
                                  functions)))
         (message (list (cons :role "user")
                        (cons :content starter-text)))
         (base-data `((model . ,*model*)
                      (messages . ,(list message))))
         (data (if function-defs
                   (append base-data (list (cons :functions function-defs)))
                   base-data))
         (request-body (cl-json:encode-json-to-string data))
         (fixed-json-data (substitute-subseq request-body ":null" ":false" :test #'string=))
         (escaped-json (escape-json fixed-json-data))
         (curl-command
          (format nil "curl ~A -H \"Content-Type: application/json\" -H \"Authorization: Bearer ~A\" -d \"~A\""
                  *model-host*
                  (uiop:getenv "OPENAI_KEY")
                  escaped-json)))
    (openai-helper curl-command))
~~~~~~~~

Note that the OpenAI API models are stochastic. When generating output words (or tokens), the model assigns probabilities to possible words to generate and samples a word using these probabilities. As a simple example, suppose given prompt text "it fell and", then the model could only generate three words, with probabilities for each word based on this prompt text:

- the 0.9
- that 0.1
- a 0.1

The model would *emit* the word **the** 90% of the time, the word **that** 10% of the time, or the word **a** 10% of the time. As a result, the model can generate different completion text for the same text prompt. Let's look at some examples. We request 22 output tokens (words or punctuation) in the first two examples and 100 tokens in the third example:

{lang="lisp",linenos=on}
~~~~~~~~
cl-user> (openai:completions "The President went to Congress")
" yesterday and proposed a single tax rate for all corporate taxpayers, which he envisions will be lower than what our"

cl-user> (openai:completions "The President went to Congress")
" last month, asking for authorization of a program, which had previously been approved by the Foreign Intelligence Surveillance court as"

cl-user> (openai:completions "The President went to Congress")
" worried about what the massive unpopular bill would do to his low approvals. Democrats lost almost every situation to discuss any legislation about this controversial subject. Even more so, President Obama failed and had to watch himself be attacked by his own party for not leading.

There were also two celebrated (in DC) pieces of student loan legislation, which aimed to make college cheaper. Harkin teamed up with Congressman Roddenbery on one, Student Loan Affordability Act, and Senator Jack Reed (D"
cl-user> 
~~~~~~~~

The function **summarize** is very similar to the function **completions** except the JSON data passed to the API has a few additional parameters that let the API know that we want a text summary:

- presence_penalty - penalize words found in the original text (we set this to zero)
- temperature - higher values the randomness used to select output tokens. If you set this to zero, then the same prompt text will always yield the same results (I never use a zero value).
- top_p - also affects randomness. All examples I have seen use a value of 1.
- frequency_penalty - penalize using the same words repeatedly (I usually set this to zero, but you should experiment with different values)

An example:

{lang="lisp",linenos=on}
~~~~~~~~
 (defvar s "Jupiter is the fifth planet from the Sun and the largest in the Solar System. It is a gas giant with a mass one-thousandth that of the Sun, but two-and-a-half times that of all the other planets in the Solar System combined. Jupiter is one of the brightest objects visible to the naked eye in the night sky, and has been known to ancient civilizations since before recorded history. It is named after the Roman god Jupiter.[19] When viewed from Earth, Jupiter can be bright enough for its reflected light to cast visible shadows,[20] and is on average the third-brightest natural object in the night sky after the Moon and Venus.")

cl-user> (openai:summarize s)
"Jupiter is a gas giant because it is predominantly composed of hydrogen and helium; it has a solid core that is composed of heavier elements. It is the largest of the four giant planets in the Solar System and the largest in the Solar System"
~~~~~~~~

Let's look at a few question answering examples and we will discuss possible problems and workarounds:

{lang="lisp",linenos=on}
~~~~~~~~
cl-user> (openai:answer-question "Where is the Valley of Kings?")
"It's in Egypt."
~~~~~~~~

Let's explore some issues with the question answering model. In the last example there is one good answer and the model works well. The next example **"What rivers are in Arizona?"** shows some problems because there are many rivers in Arizona. Sometimes the model misses a few rivers and often river names are repeated in the output. You also don't necessarily get the same answer for the same input arguments. Here is an example:

{lang="lisp",linenos=on}
~~~~~~~~
cl-user> (openai:answer-question "What rivers are in Arizona?")
"The Colorado River, the Gila River, the Little Colorado River, the Salt River, the Verde River, the San Pedro River, the Santa Cruz River, the San Juan River, the Agua Fria River, the Hassayampa River, the Bill Williams River, the Little Colorado River, the San Francisco River, the San Pedro River, the Santa Cruz River, the San Juan River, the Agua Fria River, the Hassayampa River, the Bill Williams River, the Little Colorado River, the San Francisco River, the San Pedro River, the Santa Cruz River, the San Juan River, the Agua Fria River, the Hassayampa River, the Bill Williams River, the Little Colorado River, the San Francisco River, the San Pedro River, the Santa Cruz"
~~~~~~~~


My library does not handle embedded single quote characters in questions so the question "Who is Bill Clinton's wife?" will throw an error. Leaving out the single quote character works fine:

{lang="lisp",linenos=on}
~~~~~~~~
cl-user> (openai:answer-question "Who is Bill Clintons wife?")
"Hillary Clinton."
cl-user> 
~~~~~~~~

The function **embeddings** (defined in **utils.lisp**) is used to convert a chunk of text to an embedding. What are embeddings? Embeddings take complex content like natural language words and sentences or software code and converts text into a special sequence of numbers. This process lets machines model the underlying concepts and relationships within the content, just like understanding the main ideas in a book even if you don't know every word. Embeddings are often used in RAG (Retrieval Augmented Generation) applications to work around the problem of the limits of the amount of context text a LLM can process. For example, if an LLM can only accept 8192 tokens a RAG application might “chunk” input text into 2K segments. Using embeddings with a vector store database, we could find the 3 chunks of original text that most closely match a query. The text for these three matched chunks could be supplied to a LLM as context text for answering a question of query.

{lang="lisp",linenos=on}
~~~~~~~~
(defun embeddings (text)
  "Get embeddings using text-embedding-3-small model (1536 dimensions)"
  (let* ((curl-command
          (concatenate 'string
                       "curl https://api.openai.com/v1/embeddings "
                       " -H \"Content-Type: application/json\""
                       " -H \"Authorization: Bearer " (uiop:getenv "OPENAI_KEY") "\" "
                       " -d '{\"input\": \"" text 
                       "\", \"model\": \"text-embedding-3-small\"}'"))
         (response (uiop:run-program curl-command :output :string)))
    (with-input-from-string (s response)
      (let ((json-as-list (json:decode-json s)))
        (cdr (nth 2 (cadr (cadr json-as-list))))))))
~~~~~~~~

The following output is edited for brevity:

{lang="lisp",linenos=off}
~~~~~~~~ 
CL-USER 5 > (openai::embeddings "John bought a new car")
(0.0038357755 0.007082982 -7.8207086E-4 -0.003108127 -0.038506914 ...
CL-USER 6 > (length (openai::embeddings "John bought a new car"))
1536
~~~~~~~~

In addition to reading the beta OpenAI API documentation you might want to read general material on the use of OpenAI's GPT-5 models. Since the APIs we are using are beta they may change. I will update this chapter and the source code on GitHub if the APIs change.

## History of Mistral AI

Mistral AI is a French company that is a leader in the development and utilization of Large Language Models (LLMs). The company was founded in 2018 by a team of AI experts who previously worked at Google and Meta to harness the power of language models for various applications.

I am a fan of Mistral because they supply both hosted APIs for their models as well as publicly releasing the weights for their smaller models like **mistral-7b** and **mixtral-8-7b** with commercial-friendly licensing. I run both of these models locally for much of my personal research on my Mac mini with 32G of memory. They have a larger model **mistral-medium** that is only available through their hosted API.

## Client Library for Mistral APIs

Mistral designed their API schemas to be similar to those of OPenAI so the client code for Mistral is very similar to what we saw in the previous sections.

The following code is found in the GitHub repository [https://github.com/mark-watson/mistral](https://github.com/mark-watson/mistral/tree/main):

```lisp
(in-package #:mistral)

;; define the environment variable "MISTRAL_API_KEY" with the value of your mistral API key


(defvar model-host "https://api.mistral.ai/v1/chat/completions")

;; Available models:
;;
;;   "mistral-tiny" powered by Mistral-7B-v0.2
;;   "mistral-small" powered Mixtral-8X7B-v0.1, a sparse mixture of experts model with 12B active parameters
;;   "mistral-medium" powered by a larger internal prototype model
;;

(defun mistral-helper (curl-command)
  (let ((response
          (uiop:run-program
           curl-command
           :output :string)))
    (with-input-from-string
        (s response)
      (let* ((json-as-list (json:decode-json s)))
        ;; extract text (this might change if mistral changes JSON return format):
        (cdr (assoc :content (cdr (assoc :message (cadr (assoc :choices json-as-list))))))))))

(defun completions (starter-text max-tokens)
  (let* ((d
          (cl-json:encode-json-to-string
           `((:messages . (((:role . "user") (:content . ,starter-text))))
             (:model . "mistral-small")
             (:max_tokens . ,max-tokens))))
         (curl-command
          (concatenate
           'string
           "curl " model-host
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "MISTRAL_API_KEY") "\" " 
           " -d '" d "'")))
    (mistral-helper curl-command)))

(defun summarize (some-text max-tokens)
  (let* ((curl-command
          (concatenate
           'string
           "curl " model-host
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "MISTRAL_API_KEY") "\" " 
           " -d '{\"messages\": [{\"role\": \"user\", \"content\": \"Sumarize: " some-text 
           "\"}], \"model\": \"mistral-small\", \"max_tokens\": " (write-to-string max-tokens)  "}'")))
    (mistral-helper curl-command)))

(defun answer-question (question-text max-tokens)
  (let ((q-text
          (concatenate
           'string
           "\nQ: " question-text "\nA:")))
    (completions question-text max-tokens)))

(defun embeddings (text)
  (let* ((curl-command
          (concatenate
           'string
           "curl  https://api.mistral.ai/v1/embeddings "
           " -H \"Content-Type: application/json\""
           " -H \"Authorization: Bearer " (uiop:getenv "MISTRAL_API_KEY") "\" " 
           " -d '{\"input\": [\"" text "\"], \"model\": \"mistral-embed\"}'")))
    (let ((response
           (uiop:run-program
            curl-command
            :output :string)))
      ;;(princ curl-command)
      ;;(pprint response)
      (with-input-from-string
          (s response)
        (let ((json-as-list (json:decode-json s)))
          ;; return a list of 1024 floats:
          (cdr (assoc :embedding (cadr (assoc :data json-as-list)))))))))

(defun dot-product-recursive (a b) ;; generated by Bing+ChatGPT Search
  (print "dot-product")
  (if (or (null a) (null b))
      0
      (+ (* (first a) (first b))
         (dot-product (rest a) (rest b)))))

(defun dot-product (list1 list2)
  (let ((sum 0))
    (loop for x in list1
          for y in list2
          do
               (setf sum (+ sum (* x y))))
    sum))
```

Here is a simple example using the Mistral code completion API:
	
	
```lisp
cl-user> (ql:quickload :mistral)
cl-user> (mistral:completions "The President went to Congress" 200)
"When the President of a country goes to Congress, it typically means that they are making a formal address to a joint session of the legislative body. This is often done to present the State of the Union address, which outlines the administration's goals and priorities for the upcoming year. The President may also go to Congress to propose new legislation, rally support for existing bills, or address important national issues.

During the address, members of Congress from both parties are usually present in the chamber, and they may respond with applause, standing ovations, or other forms of expression. The President's speech is typically broadcast live on television and radio, and it is covered extensively by the news media.

The practice of the President going to Congress to deliver a State of the Union address dates back to the early years of the United States, when President George Washington gave the first such address in 1790. Since then, it has become a regular tradition for"
```
