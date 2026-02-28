# Catagorize and summarize text

From my book URI: https://leanpub.com/lovinglisp

Clone this repo into ~/quicklisp/local-projects and then try:


````
(ql:quickload "categorize_summarize")

(defvar s1 "Plunging European stocks, wobbly bonds and grave concerns about
the health of Portuguese lender Banco Espirito Santo SA made last
week feel like a rerun of the euro crisis, but most investors say
it was no more than a blip for a resurgent region. Banco Espirito
Santo has been in investorsa sights since December, when The Wall
Street Journal first reported on accounting irregularities at the
complex firm. Nerves frayed on Thursday when Banco Espirito Santo's
parent company said it wouldn't be able to meet some short-term debt
obligations. B J Cole gave a concert at IBM headquarters in Canada and then in France.
I heard him on the Australian Broadcasting Corporation being
critical of Australian Broadcasting Corporation.
Story was written by Frank Munoz a member of the Australian Writers Guild
as taught at the American University.")

(defvar some-words (myutils:words-from-string s1))

(setq some-categories (categorize_summarize:categorize some-words))

(categorize_summarize:summarize some-words some-categories)
  
````

Note, this is very old code that I started working on in the 1990s.
While it has the advantage of being all Common Lisp code, I now
usually prefer using the Python spaCy library with my Common Lisp wrapper:

https://github.com/mark-watson/spacy-py4cl

