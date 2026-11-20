SBCL ?= sbcl
QUICKLISP ?= ~/quicklisp/setup.lisp
ASD := $(CURDIR)/litelm.asd
SBCL_BATCH := $(SBCL) --no-userinit --non-interactive

.PHONY: all test compile deps repl clean help

all: test

help:
	@echo "Targets:"
	@echo "  make test     - Load :litelm and run tests.lisp (default)"
	@echo "  make compile  - Force-recompile and load :litelm"
	@echo "  make deps     - Install Quicklisp dependencies (:dexador)"
	@echo "  make repl     - Start an interactive SBCL REPL with :litelm loaded"
	@echo "  make clean    - Remove local .fasl and backup files"

deps:
	$(SBCL_BATCH) \
		--eval '(load "$(QUICKLISP)")' \
		--eval '(ql:quickload :dexador)'

compile:
	$(SBCL_BATCH) \
		--eval '(load "$(QUICKLISP)")' \
		--eval '(asdf:load-asd "$(ASD)")' \
		--eval '(asdf:load-system :litelm :force t)'

test:
	$(SBCL_BATCH) \
		--eval '(load "$(QUICKLISP)")' \
		--eval '(asdf:load-asd "$(ASD)")' \
		--eval '(asdf:load-system :litelm)' \
		--load tests.lisp

repl:
	$(SBCL) \
		--eval '(load "$(QUICKLISP)")' \
		--eval '(asdf:load-asd "$(ASD)")' \
		--eval '(asdf:load-system :litelm)'

clean:
	rm -f *.fasl *~
