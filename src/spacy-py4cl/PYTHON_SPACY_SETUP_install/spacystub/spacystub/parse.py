import spacy

nlp = spacy.load("en_core_web_sm")

def parse(text):
  doc = nlp(text)
  response = {}
  response['entities'] = [(ent.text, ent.start_char, ent.end_char, ent.label_) for ent in doc.ents]
  response['tokens'] = [token.text for token in doc]
  return [response['tokens'], response['entities']]
