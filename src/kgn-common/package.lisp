;;;; package.lisp

(defpackage #:kgn-common
  (:use #:cl #:alexandria #:myutils  #:myutils #:sparql-cache
   #:entities #:entity-uris #:kbnlp)
  (:export #:kgn-common #:remove-stop-words #:entity-results->relationship-links
           #:get-entity-data-helper #:handle-URIs-in-query
           #:remove-uris-from-query #:get-URIs-in-query #:display-entity-results
           #:string-shorten #:prompt-string #:dbpedia-get-product-detail
           #:dbpedia-get-person-detail #:dbpedia-get-country-detail
           #:dbpedia-get-city-detail #:dbpedia-get-company-detail #:clean-results
           #:dbpedia-get-entities-by-name #:clean-comment))
