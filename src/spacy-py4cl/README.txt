# spaCY NLP example from my book "Loving Common Lisp, The Saavy Programmer's Secret Weapon"

https://leanpub.com/lovinglisp


## install Python library 'spacystub' - one time:

Thanks to Markus Klink for notes for using a newer version of spacCY

## THIS IS ONLY TESTED ON macOS 15!

### Installation:

As of November 2024, the only way I can run this example on my M2 Mac is in conda:

install miniconda: https://docs.anaconda.com/miniconda/miniconda-other-installer-links/

export PATH=/Users/markw/bin/miniconda3/bin/:$PATH
conda install -c conda-forge spacy
python -m spacy download en_core_web_sm

cd PYTHON_SPACY_SETUP_install

pip install -e spacystub
cd ..


## running 
try:

````
(ql:quickload "spacy-py4cl")
(spacy-py4cl:nlp "John Smith went to Mexico")
#(#("John" "Smith" "went" "to" "Mexico")
  #(("John Smith" 0 10 "PERSON") ("Mexico" 19 25 "GPE")))

(spacy-py4cl:nlp "My sister has a dog Henry. She loves him.")
(spacy-py4cl:nlp "President Bill Clinton went to Congress. He gave a speech on taxes and Mexico.")
