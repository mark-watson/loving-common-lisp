message:
	echo "copy this Makefile to ~/quicklisp/local-projects and then 'make fetch'"

fetch:
	git -C conceptnet pull || git clone git@github.com:mark-watson/conceptnet.git
	git -C kgn-common pull || git clone git@github.com:mark-watson/kgn-common.git
	git -C kgn-text-ui pull || git clone git@github.com:mark-watson/kgn-text-ui.git
	git -C kgn-capi-ui pull || git clone git@github.com:mark-watson/kgn-capi-ui.git