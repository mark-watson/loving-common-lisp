SBCL ?= sbcl
LABEL ?= baseline
DB ?= eval-runs.db

# Register this directory with ASDF so targets work without a local-projects symlink.
INIT = --eval '(require :asdf)' --eval '(asdf:load-asd (truename "llm-eval.asd"))'
QL_OR_ASDF = (if (find-package :ql) (uiop:symbol-call :ql :quickload $(1)) (asdf:load-system $(1)))

.PHONY: help test compile examples rag-eval summaries clean

help:
	@echo "Targets:"
	@echo "  make test         Run offline FiveAM unit tests"
	@echo "  make compile      Compile and load :llm-eval and :llm-eval/rag"
	@echo "  make examples     Run examples.lisp (needs Ollama models)"
	@echo "  make rag-eval     Run one RAG evaluation (LABEL=$(LABEL))"
	@echo "  make summaries    Print stored run summaries from $(DB)"
	@echo "  make clean        Remove $(DB) and local *.fasl files"

test:
	$(SBCL) --non-interactive $(INIT) \
		--eval '$(call QL_OR_ASDF,:llm-eval/tests)' \
		--eval '(unless (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :all-tests :llm-eval-tests)) (uiop:quit 1))'

compile:
	$(SBCL) --non-interactive $(INIT) \
		--eval '$(call QL_OR_ASDF,:llm-eval)' \
		--eval '$(call QL_OR_ASDF,:llm-eval/rag)'

examples:
	$(SBCL) --non-interactive $(INIT) \
		--eval '$(call QL_OR_ASDF,:llm-eval/rag)' \
		--load examples.lisp

rag-eval:
	$(SBCL) --non-interactive $(INIT) \
		--eval '$(call QL_OR_ASDF,:llm-eval/rag)' \
		--eval '(llm-eval::run-rag-eval "$(LABEL)")'

summaries:
	$(SBCL) --non-interactive $(INIT) \
		--eval '$(call QL_OR_ASDF,:llm-eval)' \
		--eval '(let ((db (llm-eval:open-store "$(DB)"))) (unwind-protect (llm-eval:print-run-summaries db) (sqlite:disconnect db)))'

clean:
	rm -f $(DB) *.fasl
