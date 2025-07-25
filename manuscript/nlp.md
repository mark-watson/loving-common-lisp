# Natural Language Processing {#nlp_chapter}

Natural Language Processing (NLP) is the automated processing of natural language text with several goals:

- Determine the parts of speech (POS tagging) of words based on the surrounding words.
- Detect if two text documents are similar.
- Categorize text (e.g., is it about the economy, politics, sports, etc.)
- Summarize text
- Determine the sentiment of text
- Detect names (e.g., place names, people's names, product names, etc.)

We will use a library that I wrote that performs POS tagging, categorization (classification), summarization, and detects proper names.

My example code for this chapter is contained in separate Quicklisp projects located in the subdirectories:

- **src/fasttag**: performs part of speech tagging and tokenizes text
- **src/categorize_summarize**: performs categorization (e.g., detects the topic of text is news, politics, economy, etc.) and text summarization
- **src/kbnlp**: the top level APIs for my pure Common Lisp natural language processing (NLP) code. In later chapters we will take a different approach by using Python deep learning models for NLP that we call as a web service. I use both approaches in my own work.

I worked on this Lisp code, and also similar code in Java, from about 2001 to 2011, and again in 2019 for my application for generating knowledge graph data automatically (this is an example in a later chapter). I am going to begin the next section with a quick explanation of how to run the example code. If you find the examples interesting then you can also read the rest of this chapter where I explain how the code works.

The approach that I used in my library for categorization (word counts) is now dated. I recommend that you consider taking Andrew Ng's course on Machine Learning on the free online Coursera system and then take one of the Coursera NLP classes for a more modern treatment of NLP.

In addition to the code for my library you might also find the linguistic data in **src/linguistic_data** useful.

## Loading and Running the NLP Library

I repackaged the NLP example code into one long file. The code used to be split over 18 source files. The code should be loaded from the **src/kbnlp** directory:

~~~~~~~~
%  loving-common-lisp git:(master) > cd src/kbnlp
%  src/kbnlp git:(master) > sbcl
* (ql:quickload "kbnlp")

"Startng to load data...." 
"....done loading data." 
*
~~~~~~~~

This also loads the projects in **src/fasttag** and **src/categorize_summarize**.

Unfortunately, it takes about a minute using SBCL to load the required linguistic data so I recommend creating a Lisp image that can be reloaded to avoid the time required to load the data: 


~~~~~~~~
* (sb-ext:save-lisp-and-die "nlp-image" :purify t)
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into nlp-image:
writing 5280 bytes from the read-only space at 0x0x20000000
writing 3088 bytes from the static space at 0x0x20100000
writing 80052224 bytes from the dynamic space at 0x0x1000000000
done]
%  src git:(master) > ls -lh nlp-image
-rw-r--r--  1 markw  staff    76M Jul 13 12:49 nlp-image
~~~~~~~~

In line 1 in this repl listing, I use the SBCL built-in function **save-lisp-and-die** to create the Lisp image file. Using **save-lisp-and-die** is a great technique to use whenever it takes a while to set up your work environment. Saving a Lisp image for use the next time you work on a Common Lisp project is reminiscent of working in Smalltalk where your work is saved between sessions in an image file.

Note: I often use Clozure-CL (CCL) instead of SBCL for developing my NLP libraries because CCL loads my data files much faster than SBCL.

You can now start SBCL with the NLP library and data preloaded using the Lisp image that you just created:

~~~~~~~~
%  src git:(master) > sbcl --core nlp-image 
* (in-package :kbnlp)

#<PACKAGE "KBNLP">
* (defvar
   *x*
   (make-text-object
     "President Bob Smith talked to Congress about the economy and taxes"))

*X*

* *X*

#S(TEXT
   :URL ""
   :TITLE ""
   :SUMMARY "<no summary>"
   :CATEGORY-TAGS (("news_politics.txt" 0.01648)
                   ("news_economy.txt" 0.01601))
   :KEY-WORDS NIL
   :KEY-PHRASES NIL
   :HUMAN-NAMES ("President Bob Smith")
   :PLACE-NAMES NIL
   :TEXT #("President" "Bob" "Smith" "talked" "to" "Congress" "about" "the"
           "economy" "and" "taxes")
   :TAGS #("NNP" "NNP" "NNP" "VBD" "TO" "NNP" "IN" "DT" "NN" "CC" "NNS")
   :STEMS #("presid" "bob" "smith" "talk" "to" "congress" "about" "the"
            "economi" "and" "tax"))
* 
~~~~~~~~

At the end of the file **src/knowledgebooks_nlp.lisp** in comments is some test code that processes much more text so that a summary is also generated; here is a bit of the output you will see if you load the test code into your repl:

~~~~~~~~
(:SUMMARY
  "Often those amendments are an effort to change government policy
   by adding or subtracting money for carrying it out. The initial
   surge in foreclosures in 2007 and 2008 was tied to subprime
   mortgages issued during the housing boom to people with shaky
   credit. 2 trillion in annual appropriations bills for funding
   most government programs — usually low profile legislation that
   typically dominates the work of the House in June and July.
   Bill Clinton said that banking in Europe is a good business.
   These days homeowners who got fixed rate prime mortgages because
   they had good credit cannot make their payments because they are
   out of work. The question is whether or not the US dollar remains
   the world s reserve currency if not the US economy will face
   a depression."
:CATEGORY-TAGS (("news_politics.txt" 0.38268)
                ("news_economy.txt" 0.31182)
                ("news_war.txt" 0.20174))
:HUMAN-NAMES ("President Bill Clinton")
:PLACE-NAMES ("Florida"))
~~~~~~~~

The top-level function **make-text-object** takes one required argument that can be either a string containing text or an array of strings where each string is a word or punctuation. Function **make-text-object** has two optional keyword parameters: the URL where the text was found and a title.

{lang="lisp",linenos=on}
~~~~~~~~
(defun make-text-object (words &key (url "") (title ""))
  (if (typep words 'string) (setq words (words-from-string words)))
  (let* ((txt-obj (make-text :text words :url url :title title)))
    (setf (text-tags txt-obj) (part-of-speech-tagger words))
    (setf (text-stems txt-obj) (stem-text txt-obj))
    ;; note: we must find human and place names before calling
    ;; pronoun-resolution:
    (let ((names-places (find-names-places txt-obj)))
      (setf (text-human-names txt-obj) (car names-places))
      (setf (text-place-names txt-obj) (cadr names-places)))
    (setf (text-category-tags txt-obj)
          (mapcar
            #'(lambda (x)
                (list
                  (car x)
                  (/ (cadr x) 1000000.0)))
            (get-word-list-category (text-text txt-obj))))
    (setf (text-summary txt-obj) (summarize txt-obj))
    txt-obj))
~~~~~~~~

In line 2, we check if this function was called with a string containing text in which case the function **words-from-string** is used to tokenize the text into an array of string tokens. Line two defines the local variable **txt-obj** with the value of a new text object with only three slots (attributes) defined: **text**, **url**, and **title**. Line 4 sets the slot **text-tags** to the part of speech tokens using the function **part-of-speech-tagger**. We use the function **find-names-places** in line 8 to get person and place names and store these values in the text object. In lines 11 through 17 we use the function **get-word-list-category** to set the categories in the text object. In line 18 we similarly use the function **summarize** to calculate a summary of the text and also store it in the text object. We will discuss these NLP helper functions throughout the rest of this chapter.

The function **make-text-object** returns a struct that is defined as:

{lang="lisp",linenos=off}
~~~~~~~~
(defstruct text
  url
  title
  summary
  category-tags
  key-words
  key-phrases
  human-names
  place-names
  text
  tags
  stems)
~~~~~~~~


## Part of Speech Tagging

This tagger is the Common Lisp implementation of my FastTag open source project. I based this project on Eric Brill's PhD thesis (1995). He used machine learning on annotated text to learn tagging rules. I used a subset of the tagging rules that he generated that were most often used when he tested his tagger. I hand coded his rules in Lisp (and Ruby, Java, and Pascal). My tagger is less accurate, but it is fast - thus the name FastTag.

If you just need part of speech tagging (and not summarization, categorization, and top level APIs used in the last section) you can load:


~~~~~~~~
(ql:quickload "fasttag")
~~~~~~~~

You can find the tagger implementation in the function **part-of-speech-tagger**. We already saw sample output from the tagger in the last section:

~~~~~~~~
:TEXT #("President" "Bob" "Smith" "talked" "to" "Congress" "about" "the"
        "economy" "and" "taxes")
:TAGS #("NNP" "NNP" "NNP" "VBD" "TO" "NNP" "IN" "DT" "NN" "CC" "NNS")
~~~~~~~~

The following table shows the meanings of the tags and a few example words:

|Tag  |Definition    |Example words|
|---  |----------    |-------------|
|CC |Coord Conjuncn           |and, but, or|
|NN |Noun, sing. or mass      |dog|
|CD |Cardinal number          |one, two|
|NNS |Noun, plural            |dogs, cats|
|DT |Determiner               |the, some|
|NNP |Proper noun, sing.      |Edinburgh|
|EX |Existential there        |there|
|NNPS |Proper noun, plural    |Smiths|
|FW |Foreign Word             |mon dieu|
|PDT |Predeterminer           |all, both|
|IN |Preposition              |of, in, by|
|POS |Possessive ending       |’s|
|JJ |Adjective                |big|
|PP |Personal pronoun         |I, you, she|
|JJR |Adj., comparative       |bigger|
|PP$ |Possessive pronoun      |my, one’s|
|JJS |Adj., superlative       |biggest|
|RB |Adverb                   |quickly|
|LS |List item marker         |1, One|
|RBR |Adverb, comparative     |faster|
|MD |Modal                    |can, should|
|RBS |Adverb, superlative     |fastest|
|RP |Particle                 |up, off|
|WP$ |Possessive-Wh           |whose|
|SYM |Symbol                  |+, %, &|
|WRB |Wh-adverb               |how, where|
|TO |“to”                     |to|
|$ |Dollar sign               |$|
|UH |Interjection             |oh, oops|
|\# |Pound sign                |\#|
|VB |verb, base form          |eat, run|
|" |quote                     |"|
|VBD |verb, past tense        |ate|
|VBG |verb, gerund            |eating|
|( |Left paren                |(|
|VBN |verb, past part         |eaten|
|) |Right paren               |)|
|VBP |Verb, present           |eat|
|, |Comma                     |,|
|VBZ |Verb, present           |eats|
|. |Sent-final punct          |. ! ?|
|WDT |Wh-determiner           |which, that|
|: |Mid-sent punct.           |: ; —|
|WP |Wh pronoun               |who, what|

The function **part-of-speech-tagger** loops through all input words and initially assigns the most likely part of speech as specified in the lexicon. Then a subset of Brill's rules are applied. Rules operate on the current word and the previous word.

As an example Common Lisp implementation of a rule, look for words that are tagged as common nouns, but end in "ing" so they should be a gerand (verb form):

{lang="lisp",linenos=off}
~~~~~~~~
  ; rule 8: convert a common noun to a present 
  ;         participle verb (i.e., a gerand)
  (if (equal (search "NN" r) 0)
    (let ((i (search "ing" w :from-end t)))
      (if (equal i (- (length w) 3))
          (setq r "VBG"))))
~~~~~~~~

You can find the lexicon data in the file **src/linguistic_data/FastTagData.lisp**. This file is List code instead of plain data (that in retrospect would be better because it would load faster) and looks like:

{lang="lisp",linenos=off}
~~~~~~~~
(defvar lex-hash (make-hash-table :test #'equal :size 110000))
(setf (gethash "shakeup" lex-hash) (list "NN"))
(setf (gethash "Laurance" lex-hash) (list "NNP"))
(setf (gethash "expressing" lex-hash) (list "VBG"))
(setf (gethash "citybred" lex-hash) (list "JJ"))
(setf (gethash "negative" lex-hash) (list "JJ" "NN"))
(setf (gethash "investors" lex-hash) (list "NNS" "NNPS"))
(setf (gethash "founding" lex-hash) (list "NN" "VBG" "JJ"))
~~~~~~~~

I generated this file automatically from lexicon data using a small Ruby script. Notice that words can have more than one possible part of speech. The most common part of speech for a word is the first entry in the lexicon.


## Categorizing Text

The code to categorize text is fairly simple using a technique often called "bag of words." I collected sample text in several different categories and for each category (like politics, sports, etc.) I calculated the evidence or weight that words contribute to supporting a category. For example, the word "president" has a strong weight for the category "politics" but not for the category "sports." The reason is that the word "president" occurs frequently in articles and books about politics. The data file that contains the word weightings for each category is **src/data/cat-data-tables.lisp**. You can look at this file; here is a very small part of it:

If you only need categorization and not the other libraries developed in this chapter, you can just load this library and run the example in the comment at the bottom of the file **categorize_summarize.lisp**:

({lang="lisp",linenos=off}
~~~~~~~~
(ql:quickload "categorize_summarize")
(defvar x "President Bill Clinton <<2 pages text no shown>> ")
(defvar words1 (myutils:words-from-string x))
(print words1)
(setq cats1 (categorize_summarize:categorize words1))
(print cats1)
(defvar sum1 (categorize_summarize:summarize words1 cats1))
(print sum1)
~~~~~~~~

Let's look at the implementation, starting with creating hash tables for storing word count data for each category or topic:

{lang="lisp",linenos=off}
~~~~~~~~
;;;  Starting topic: news_economy.txt

(setf *h* (make-hash-table :test #'equal :size 1000))

  (setf (gethash "news" *h*) 3915)
  (setf (gethash "debt" *h*) 3826)
  (setf (gethash "money" *h*) 1809)
  (setf (gethash "work" *h*) 1779)
  (setf (gethash "business" *h*) 1631)
  (setf (gethash "tax" *h*) 1572)
  (setf (gethash "poverty" *h*) 1512)
~~~~~~~~

This file was created by a simple Ruby script (not included with the book's example code) that processes a list of sub-directories, one sub-directory per category. The following listing shows the implementation of function **get-word-list-category** that calculates category tags for input text:

{lang="lisp",linenos=on}
~~~~~~~~
(defun get-word-list-category (words)
  (let ((x nil)
        (ss nil)
        (cat-hash nil)
        (word nil)
        (len (length words))
        (num-categories (length categoryHashtables))
        (category-score-accumulation-array
          (make-array num-categories :initial-element 0)))

    (defun list-sort (list-to-sort)
      ;;(pprint list-to-sort)
      (sort list-to-sort
	    #'(lambda (list-element-1 list-element-2)
		(> (cadr list-element-1) (cadr list-element-2)))))

    (do ((k 0 (+ k 1)))
        ((equal k len))
      (setf word (string-downcase (aref words k)))
      (do ((i 0 (+ i 1)))
          ((equal i num-categories))
        (setf cat-hash (nth i categoryHashtables))
        (setf x (gethash word cat-hash))
        (if x
            (setf 
              (aref category-score-accumulation-array i)
              (+ x (aref category-score-accumulation-array i))))))
    (setf ss '())
    (do ((i 0 (+ i 1)))
        ((equal i num-categories))
      (if (> (aref category-score-accumulation-array i) 0.01)
          (setf
            ss
            (cons
              (list
                (nth i categoryNames)
                (round (* (aref category-score-accumulation-array i) 10)))
              ss))))
    (setf ss (list-sort ss))
    (let ((cutoff (/ (cadar ss) 2))
          (results-array '()))
      (dolist (hit ss)
        (if (> (cadr hit) cutoff)
            (setf results-array (cons hit results-array))))
      (reverse results-array))))
~~~~~~~~

On thing to notice in this listing is lines 11 through 15 where I define a nested function **list-sort** that takes a list of sub-lists and sorts the sublists based on the second value (which is a number) in the sublists. I often nest functions when the "inner" functions are only used in the "outer" function.

Lines 2 through 9 define several local variables used in the outer function. The global variable **categoryHashtables** is a list of word weighting score hash tables, one for each category. The local variable **category-score-accumulation-array** is initialized to an array containing the number zero in each element and will be used to "keep score" of each category. The highest scored categories will be the return value for the outer function.

Lines 17 through 27 are two nested loops. The outer loop is over each word in the input word array. The inner loop is over the number of categories. The logic is simple: for each word check to see if it has a weighting score in each category's word weighting score hash table and if it is, increment the matching category's score.

The local variable **ss** is set to an empty list on line 28 and in the loop in lines 29 through 38 I am copying over categories and their scores when the score is over a threshold value of 0.01. We sort the list in **ss** on line 39 using the inner function and then return the categories with a score greater than the median category score.


## Detecting People's Names and Place Names

The code for detecting people and place names is in the top level API code in the package defined in **src/kbnlp**. This package is loaded using:

{lang="lisp",linenos=off}
~~~~~~~~
(ql:quickload "kbnlp")
(kbnlp:make-text-object "President Bill Clinton ran for president of the USA")
~~~~~~~~


The functions that support identifying people's names and place names in text are in the Common Lisp package **kb nlp:**:

- find-names (words tags exclusion-list) -- words is an array of strings for the words in text, tags are the parts of speech tags (from FastTag), and the exclusion list is a an array of words that you want to exclude from being considered as parts of people's names. The list of found names records starting and stopping indices for names in the array words.
-  not-in-list-find-names-helper (a-list start end) -- returns true if a found name is not already been added to a list for saving people's names in text
- find-places (words exclusion-list) -- this is similar to find-names, but it finds place names.  The list of found place names records starting and stopping indices for place names in the array words.
- not-in-list-find-places-helper (a-list start end) -- returns true if a found place name is not already been added to a list for saving place names in text
- build-list-find-name-helper (v indices) -- This converts lists of start/stop word indices to strings containing the names
- find-names-places (txt-object) -- this is the top level function that your application will call. It takes a **defstruct** **text** object as input and modifies the **defstruct** **text** by adding people's and place names it finds in the text. You saw an example of this earlier in this chapter.

I will let you read the code and just list the top level function:

{lang="lisp",linenos=on}
~~~~~~~~
(defun find-names-places (txt-object)
  (let* ((words (text-text txt-object))
	 (tags (text-tags txt-object))
	 (place-indices (find-places words nil))
	 (name-indices (find-names words tags place-indices))
	 (name-list
	   (remove-duplicates
	     (build-list-find-name-helper words name-indices) :test #'equal))
	 (place-list
	   (remove-duplicates
	     (build-list-find-name-helper words place-indices) :test #'equal)))
    (let ((ret '()))
      (dolist (x name-list)
	(if (search " " x)
	    (setq ret (cons x ret))))
      (setq name-list (reverse ret)))
    (list
     (remove-shorter-names name-list)
     (remove-shorter-names place-list))))
~~~~~~~~

In line 2 we are using the slot accessor **text-text** to fetch the array of word tokens from the text object. In lines 3, 4, and 5 we are doing the same for part of speech tags, place name indices in the words array, and person names indices in the words array.

In lines 6 through 11 we are using the function **build-list-find-name-helper** twice to construct the person names and place names as strings given the indices in the words array. We are also using the Common Lisp built-in function **remove-duplicates** to get rid of duplicate names.

In lines 12 through 16 we are discarding any persons names that do not contain a space, that is, only keep names that are at least two word tokens. Lines 17 through 19 define the return value for the function: a list of lists of people and place names using the function **remove-shorter-names** twice to remove shorter versions of the same names from the lists. For example, if we had two names "Mr. John Smith" and "John Smith" then we would want to drop the shorter name "John Smith" from the return list.
 

## Summarizing Text

The code for summarizing text is located in the directory **src/categorize_summarize** and can be loaded using:

({lang="lisp",linenos=off}
~~~~~~~~
(ql:quickload "categorize_summarize")
~~~~~~~~

The code for summarization depends on the categorization code we saw earlier.

There are many applications for summarizing text. As an example, if you are writing a document management system you will certainly want to use something like Solr to provide search functionality. Solr will return highlighted matches in snippets of indexed document field values. Using summarization, when you add documents to a Solr (or other) search index you could create a new unindexed field that contains a document summary. Then when the users of your system see search results they will see the type of highlighted matches in snippets they are used to seeing in Google, Bing, or DuckDuckGo search results, and, they will see a summary of the document.

Sounds good? The problem to solve is getting good summaries of text and the technique used may have to be modified depending on the type of text you are trying to summarize. There are two basic techniques for summarization: a practical way that almost everyone uses, and an area of research that I believe has so far seen little practical application. The techniques are sentence extraction and abstraction of text into a shorter form by combining and altering sentences. We will use sentence extraction.

How do we choose which sentences in text to extract for the summary? The idea I had in 1999 was simple. Since I usually categorize text in my NLP processing pipeline why not use the words that gave the strongest evidence for categorizing text, and find the sentences with the largest number of these words. As a concrete example, if I categorize text as being "politics", I identify the words in the text like "president", "congress", "election", etc. that triggered the "politics" classification, and find the sentences with the largest concentrations of these words.

Summarization is something that you will probably need to experiment with depending on your application. My old summarization code contained a lot of special cases, blocks of commented out code, etc. I have attempted to shorten and simplify my old summarization code for the purposes of this book as much as possible and still maintain useful functionality.

The function for summarizing text is fairly simple because when the function **summarize** is called by the top level NLP library function **make-text-object**, the input text has already been categorized. Remember from the example at the beginning of the chapter that the category data looks like this:

~~~~~~~~
:CATEGORY-TAGS (("news_politics.txt" 0.38268)
                ("news_economy.txt" 0.31182)
                ("news_war.txt" 0.20174))
~~~~~~~~

This category data is saved in the local variable **cats** on line 4 of the following listing.

{lang="lisp",linenos=on}
~~~~~~~~
(defun summarize (txt-obj)
  (let* ((words (text-text txt-obj))
         (num-words (length words))
         (cats (text-category-tags txt-obj))
         (sentence-count 0)
         best-sentences sentence (score 0))
    ;; loop over sentences:
    (dotimes (i num-words)
      (let ((word (svref words i)))
        (dolist (cat cats)
          (let* ((hash (gethash (car cat) categoryToHash))
                 (value (gethash word hash)))
            (if value
                (setq score (+ score (* 0.01 value (cadr cat)))))))
        (push word sentence)
        (if (or (equal word ".") (equal word "!") (equal word ";"))
            (let ()
              (setq sentence (reverse sentence))
              (setq score (/ score (1+ (length sentence))))
              (setq sentence-count (1+ sentence-count))
              (format t "~%~A : ~A~%" sentence score)
              ;; process this sentence:
              (if (and
                   (> score 0.4)
                   (> (length sentence) 4)
                   (< (length sentence) 30))
                  (progn
                    (setq sentence
                          (reduce
                           #'(lambda (x y) (concatenate 'string x " " y))
                           (coerce sentence 'list)))
                    (push (list sentence score) best-sentences)))
              (setf sentence nil score 0)))))
    (setf
     best-sentences
     (sort
      best-sentences
      #'(lambda (x y) (> (cadr x) (cadr y)))))
    (if best-sentences
        (replace-all
         (reduce #'(lambda (x y) (concatenate 'string x " " y))
                 (mapcar #'(lambda (x) (car x)) best-sentences))
         " ." ".")
        "<no summary>")))
~~~~~~~~

The nested loops in lines 8 through 33 look a little complicated, so let's walk through it. Our goal is to calculate an importance score for each word token in the input text and to then select a few sentences containing highly scored words. The outer loop is over the word tokens in the input text. For each word token we loop over the list of categories, looking up the current word in each category hash and incrementing the score for the current word token. As we increment the word token scores we also look for sentence breaks and save sentences.

The complicated bit of code in lines 16 through 32 where I construct sentences and their scores, and store sentences with a score above a threshold value in the list **best-sentences**. After the two nested loops, in lines 34 through 44 we simply sort the sentences by score and select the "best" sentences for the summary. The extracted sentences are no longer in their original order, which can have strange effects, but I like seeing the most relevant sentences first. 


## Text Mining

Text mining in general refers to finding data in unstructured text. We have covered several text mining techniques in this chapter:

- Named entity recognition - the NLP library covered in this chapter recognizes person and place entity names. I leave it as an exercise for you to extend this library to handle company and product names. You can start by collecting company and product names in the files **src/kbnlp/linguistic_data/names/names.companies** and **src/kbnlp/data/names/names.products** and extend the library code.
- Categorizing text - you can increase the accuracy of categorization by adding more weighted words/terms that support categories. If you are already using Java in the systems you build, I recommend the Apache OpenNLP library that is more accurate than the simpler "bag of words" approach I used in my Common Lisp NLP library. If you use Python, then I recommend that you also try the NLTK library.
- Summarizing text.

In the next chapter I am going to cover another "data centric" topic: performing information gathering on the web. You will likely find some synergy between being able to use NLP to create structured data from unstructured text.
