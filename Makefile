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
	git -C fasttag pull || git clone git@github.com:mark-watson/fasttag.git
	git -C kbnlp pull || git clone git@github.com:mark-watson/kbnlp.git
	git -C myutils pull || git clone git@github.com:mark-watson/myutils.git
	git -C entities pull || git clone git@github.com:mark-watson/entitiesl.git
	git -C openai pull || git clone git@github.com:mark-watson/openai.git
	git -C anthropic pull || git clone git@github.com:mark-watson/anthropic.git
	git -C mistral pull || git clone git@github.com:mark-watson/mistral.git
	git -C kgsampler pull || git clone git@github.com:mark-watson/kgsampler.git
	git -C entities_dbpedia pull || git clone git@github.com:mark-watson/entities_dbpedia.git
	git -C huggingface pull || git clone git@github.com:mark-watson/huggingface.git
	git -C docs-qa pull || git clone git@github.com:mark-watson/docs-qa.git
	git -C Agent_CL pull || git clone git@github.com:mark-watson/Agent_CL.git
