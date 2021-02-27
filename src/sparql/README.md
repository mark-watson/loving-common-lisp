# Simple portable REST SPARQL clients for Fuseki, GraphDB, and AllegroGraph (as of October 2020: this code is not yet in my book)

There are comments at the bottom of sparql.lisp with example queries.

To run in a Common Lisp repl:

    (ql:quickload "sparql")

## Setting up Apache Jena Fuseki SPARQL endpoint for Development

Clone my github repo and follow README directions:

    https://github.com/mark-watson/fuseki-semantic-web-dev-setup
    
## Setting up AllegroGraph Free Edition  SPARQL endpoint for Development

AllegroGraph on my Ubuntu VPS

  cd ~/agraph
  bin/agraph-control start

Runs as a Daemon

http://127.0.0.1:10035

Loading data into “news” repo:

  bin/agtool load --input ntriples news sample_news.nt

On MacBook:

ssh -L 10035:localhost:10035 MY-SERVER.COM

On MacBook, access:  http://127.0.0.1:10035/#

NOTE: one time only: use **admin** menu to create account "anonymous" with no password.

To stop server on VPS:

  cd ~/agraph
  bin/agraph-control stop

Remote CURL:

curl "http://127.0.0.1:10035/repositories/news/statements"

# Notes on Stardog client

The default value for the key argument `suffix "/testdb/query"` specifies the use of database `testdb` that you can replace,
being sure to add `/query` to whatever database name you specify. (I will fix this sometime).