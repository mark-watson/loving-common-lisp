This is the stub README.txt for the "coref" project.

try:

````
(ql:quickload "spacy-web-client")
(spacy-web-client:spacy-client "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico.")

(defvar x (spacy-web-client:spacy-client "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico."))
(spacy-web-client:spacy-data-entities x)
(spacy-web-client:spacy-data-tokens x)
````
        
note: https is not working on ABCL or Clozure CCL


