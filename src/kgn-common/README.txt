This is the common utility library for the Knowledge Graph Navigator project.

keywords: SPARQL, DBPedia, linked data

Book URI: https://leanpub.com/lovinglisp

Clone this repo into ~/quicklisp/local-projects and then try:

    (ql:quickload "kgn-common")
    (kgn-common:dbpedia-get-company-detail "<http://dbpedia.org/resource/Microsoft>")
    (kgn-common::get-name-and-description-for-uri "<http://dbpedia.org/resource/Apple_Inc.>")

