from __future__ import print_function

from urllib.request import Request, urlopen
import urllib
import json

base_uri = 'http://127.0.0.1:8008?text='


def spacy_client(text, no_detail=False):
  def get_raw_data_from_web(a_uri):
    req = Request(a_uri, headers={'User-Agent': 'PythonBook/1.0'})
    http_response = urlopen(req)
    data = http_response.read()
    return data

  encoded_text = urllib.parse.quote(text)
  raw_data = get_raw_data_from_web(base_uri + encoded_text)
  return json.loads(raw_data.decode("UTF8"))


print(spacy_client('Bill Clinton went to the Pepsi factory in Mexico.'))
print(spacy_client('The Mark Jones ran. Sam Bell ran faster.'))
