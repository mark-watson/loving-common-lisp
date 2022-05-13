message:
	echo "copy this Makefile to ~/quicklisp/local-projects and then 'make fetch'"

fetch:
	git -C conceptnet pull || git clone git@github.com:mark-watson/conceptnet.git
	git -C kgn-common pull || git clone git@github.com:mark-watson/kgn-common.git
	git -C kgn-text-ui pull || git clone git@github.com:mark-watson/kgn-text-ui.git
	git -C kgn-capi-ui pull || git clone git@github.com:mark-watson/kgn-capi-ui.git
	git -C dbpedia pull || git clone git@github.com:mark-watson/dbpedia.git
	git -C bing pull || git clone git@github.com:mark-watson/bing.git
	git -C categorize_summarize pull || git clone git@github.com:mark-watson/categorize_summarize.git
	git -C entities pull || git clone git@github.com:mark-watson/entities.git
	git -C entity-uris pull || git clone git@github.com:mark-watson/entity-uris.git
	git -C sparql pull || git clone git@github.com:mark-watson/sparql.git
	git -C sparql-cache pull || git clone git@github.com:mark-watson/sparql-cache.git
