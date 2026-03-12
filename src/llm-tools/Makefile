# Makefile for Common Lisp LLM Tools
# Builds standalone SBCL executables for use with LM Studio, gemini-clj, and Claude Code.

SBCL     := sbcl
BIN_DIR  := ./bin
TMP_DIR  := ./tmp

.PHONY: all current-time files-in-current-directory clean dirs

all: current-time files-in-current-directory

dirs:
	@mkdir -p $(BIN_DIR) $(TMP_DIR)

## ─── current-time ────────────────────────────────────────────────────────────
## Saves ./tmp/current-time.core and ./bin/current-time (compressed executable).

current-time: dirs
	$(SBCL) --no-userinit --no-sysinit \
	  --eval "(load \"current-time.lisp\")" \
	  --eval "(in-package #:current-time)" \
	  --eval "(sb-ext:save-lisp-and-die \"$(TMP_DIR)/current-time.core\" \
	            :toplevel #'current-time:main \
	            :executable nil \
	            :compression t)" \
	  || true
	$(SBCL) --no-userinit --no-sysinit \
	  --eval "(load \"current-time.lisp\")" \
	  --eval "(in-package #:current-time)" \
	  --eval "(sb-ext:save-lisp-and-die \"$(BIN_DIR)/current-time\" \
	            :toplevel #'current-time:main \
	            :executable t \
	            :compression t)"

## ─── files-in-current-directory ──────────────────────────────────────────────
## Saves ./tmp/files-in-current-directory.core and ./bin/files-in-current-directory.

files-in-current-directory: dirs
	$(SBCL) --no-userinit --no-sysinit \
	  --eval "(require :asdf)" \
	  --eval "(load \"files-in-current-directory.lisp\")" \
	  --eval "(in-package #:files-in-current-directory)" \
	  --eval "(sb-ext:save-lisp-and-die \"$(TMP_DIR)/files-in-current-directory.core\" \
	            :toplevel #'files-in-current-directory:main \
	            :executable nil \
	            :compression t)" \
	  || true
	$(SBCL) --no-userinit --no-sysinit \
	  --eval "(require :asdf)" \
	  --eval "(load \"files-in-current-directory.lisp\")" \
	  --eval "(in-package #:files-in-current-directory)" \
	  --eval "(sb-ext:save-lisp-and-die \"$(BIN_DIR)/files-in-current-directory\" \
	            :toplevel #'files-in-current-directory:main \
	            :executable t \
	            :compression t)"

clean:
	rm -f $(BIN_DIR)/current-time \
	      $(BIN_DIR)/files-in-current-directory \
	      $(TMP_DIR)/current-time.core \
	      $(TMP_DIR)/files-in-current-directory.core
