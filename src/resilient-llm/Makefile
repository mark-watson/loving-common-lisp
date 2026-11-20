SBCL ?= sbcl
LOAD_ASD = (asdf:load-asd (truename "resilient-llm.asd"))

.PHONY: all test examples repl clean help

all: test

test:
	$(SBCL) --non-interactive \
	  --eval '$(LOAD_ASD)' \
	  --eval '(ql:quickload :resilient-llm/tests :silent t)' \
	  --eval '(unless (resilient-llm::run-tests) (uiop:quit 1))'

examples:
	$(SBCL) --non-interactive \
	  --eval '$(LOAD_ASD)' \
	  --eval '(ql:quickload :resilient-llm :silent t)' \
	  --eval '(load "examples.lisp")'

repl:
	$(SBCL) \
	  --eval '$(LOAD_ASD)' \
	  --eval '(ql:quickload :resilient-llm)' \
	  --eval '(in-package #:resilient-llm)'

clean:
	rm -f *.fasl

help:
	@echo "Targets:"
	@echo "  make test      Run the offline test suite (default)"
	@echo "  make examples  Run live examples against a local Ollama server"
	@echo "  make repl      Start an SBCL REPL with :resilient-llm loaded"
	@echo "  make clean     Remove compiled .fasl files"
