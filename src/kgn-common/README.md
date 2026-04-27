# Knowledge Graph Navigator — Common Utilities

**Book Chapter:** [Knowledge Graph Navigator Common Code and NLP Utilities](https://leanpub.com/read/lovinglisp/knowledge-graph-navigator-common-code-and-nlp-utilities) — *Loving Common Lisp* (free to read online).

Shared library used by both the CAPI (GUI) and text-based front-ends of the Knowledge Graph Navigator. It provides functions for:

- Querying DBpedia for detailed information about people, companies, countries, cities, and products
- Extracting and resolving entities in user queries to DBpedia URIs
- Cleaning and shortening comment strings from SPARQL results
- Managing a local SQLite cache of SPARQL query results
- Removing stop words from text

## Prerequisites

- **SBCL** with [Quicklisp](https://www.quicklisp.org/)
- Sibling libraries: `myutils`, `entities`, `entity-uris`, `kbnlp`, `sparql-cache`

## Dependencies

- `sqlite`, `cl-json`, `alexandria`, `drakma`, `myutils`, `entities`, `entity-uris`, `kbnlp`, `sparql-cache`

## Usage

```lisp
(ql:quickload "kgn-common")

;; Look up company details from a DBpedia URI
(kgn-common:dbpedia-get-company-detail "<http://dbpedia.org/resource/Microsoft>")

;; Get name and description for any DBpedia entity
(kgn-common::get-name-and-description-for-uri "<http://dbpedia.org/resource/Apple_Inc.>")
```

## Key Exported Functions

- `dbpedia-get-person-detail`, `dbpedia-get-company-detail`, `dbpedia-get-country-detail`, `dbpedia-get-city-detail`, `dbpedia-get-product-detail` — Fetch detailed SPARQL data for specific entity types.
- `handle-URIs-in-query`, `get-URIs-in-query`, `remove-uris-from-query` — Parse and manipulate URIs embedded in user queries.
- `remove-stop-words` — Filter noise words from a word list.
- `clean-comment`, `string-shorten` — Clean and truncate DBpedia descriptions.

## Data Files

The `data/` subdirectory contains stop-word lists used for text filtering.
