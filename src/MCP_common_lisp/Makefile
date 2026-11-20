SBCL ?= sbcl
ASD := $(CURDIR)/mcp-common-lisp.asd
BINARY := mcp-server

.PHONY: help run test compile build clean

help:
	@echo "Targets:"
	@echo "  make test     Run the non-destructive MCP stdio test session (./test-session.sh)"
	@echo "  make run      Start the MCP server on stdio (./run-server.sh)"
	@echo "  make compile  Load and compile :mcp-common-lisp via SBCL + Quicklisp"
	@echo "  make build    Build a standalone '$(BINARY)' executable"
	@echo "  make clean    Remove build artifacts ($(BINARY), *.fasl)"

test:
	./test-session.sh

run:
	./run-server.sh

compile:
	$(SBCL) --noinform --non-interactive \
		--eval '(asdf:load-asd "$(ASD)")' \
		--eval '(ql:quickload :mcp-common-lisp :silent t)' \
		--eval '(quit)'

build:
	$(SBCL) --noinform --non-interactive \
		--eval '(asdf:load-asd "$(ASD)")' \
		--eval '(ql:quickload :mcp-common-lisp :silent t)' \
		--eval '(sb-ext:save-lisp-and-die "$(BINARY)" :toplevel #'\''mcp-server:start :executable t)'

clean:
	rm -f $(BINARY) *.fasl
