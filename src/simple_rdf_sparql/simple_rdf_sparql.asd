(asdf:defsystem #:simple_rdf_sparql
  :description "Simple RDF datastore with partial SPARQL support"
  :author "Mark Watson <markw@markwatson.com>"
  :license "Apache 2"
  ;;depends-on ()
  :components ((:file "package")
               (:file "simple_rdf_sparql")))
