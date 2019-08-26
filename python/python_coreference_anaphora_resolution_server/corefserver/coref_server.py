#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Coreference resolution server example.
A simple server serving the coreference system.

This file is copied and modified from an example
program from https://github.com/huggingface/neuralcoref

"""
from __future__ import unicode_literals
from __future__ import print_function

import json
from wsgiref.simple_server import make_server
import falcon
import spacy

unicode_ = str      # Python 3


class AllResource(object):
    def __init__(self):
        #self.nlp = spacy.load('en_coref_sm')
        # MLW:
        self.nlp = spacy.load('en')
        import neuralcoref
        neuralcoref.add_to_pipe(self.nlp)
        print("Server loaded")
        self.response = None

    def on_get(self, req, resp):
        self.response = {}

        text_param = req.get_param("text")
        no_detail  = req.get_param("no_detail")
        if text_param is not None:
            
            text = ",".join(text_param) if isinstance(text_param, list) else text_param
            text = unicode_(text)
            text = text.replace("%20", " ").replace("%3B", ";").replace("%2C", ",").replace("%3A", ":").replace("%24","$").replace("%2C",",")
            print("** text=", text)
            doc = self.nlp(text)
            #print("** doc=", doc)
            if no_detail != None:
                self.response = doc._.coref_resolved
                resp.body = self.response
                resp.content_type = 'application/text'
            else:
                if doc._.has_coref:
                  mentions = [{'start':    mention.start_char,
                               'end':      mention.end_char,
                               'text':     mention.text,
                               'resolved': cluster.main.text
                              }
                              for cluster in doc._.coref_clusters
                              for mention in cluster.mentions]
                  clusters = list(list(span.text for span in cluster)
                                  for cluster in doc._.coref_clusters)
                  resolved = doc._.coref_resolved
                  self.response['mentions'] = mentions
                  self.response['clusters'] = clusters
                  self.response['resolved'] = resolved

                  resp.body = json.dumps(self.response)
                  resp.content_type = 'application/json'
        resp.append_header('Access-Control-Allow-Origin', "*")
        resp.status = falcon.HTTP_200

if __name__ == '__main__':
    RESSOURCE = AllResource()
    APP = falcon.API()
    APP.add_route('/', RESSOURCE)
    HTTPD = make_server('0.0.0.0', 8000, APP)
    HTTPD.serve_forever()
