# install Python library 'spacystub' - one time

cd PYTHON_SPACY_SETUP_install/spacystub
python setup.py install

# running 
try:

````
(ql:quickload "spacy-py4cl")
(spacy-py4cl:nlp "John Smith went to Mexico")
#(#("John" "Smith" "went" "to" "Mexico")
  #(("John Smith" 0 10 "PERSON") ("Mexico" 19 25 "GPE")))

(spacy-py4cl:nlp "My sister has a dog Henry. She loves him.")
(spacy-py4cl:nlp "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico.")
