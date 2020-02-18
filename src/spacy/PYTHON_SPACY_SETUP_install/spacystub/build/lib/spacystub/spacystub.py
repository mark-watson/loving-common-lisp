import spacy

nlp = spacy.load("en")

def parse(text):
  return nlp(text)
