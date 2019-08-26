This is the stub README.txt for the "coref" project.

try:

````
(ql:quickload "spacy")
(spacy:spacy-client "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico.")

(defvar x (spacy:spacy-client "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico."))
(spacy:spacy-data-entities x)
(spacy:spacy-data-tokens x)
````
        
note: https is not working on ABCL or Clozure CCL


