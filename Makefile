SRC   := $(wildcard src/*.el)
BIN   := $(subst .el,.elc,$(SRC))
EMACS ?= emacs --eval '(add-to-list (quote load-path) (concat default-directory "src/"))'

.PHONY: ci/cd
ci/cd: lint test

.PHONY: lint
lint: compile checkdoc

.PHONY: checkdoc
checkdoc: ## Check for missing or poorly formatted docstrings
	@for file in $(SRC); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: compile
compile: ## Check for byte-compiler errors
	@for file in $(SRC); do \
	    echo "[compile] $$file" ;\
	    [ -e "$${file}c" ] && rm -f "$${file}c" ;\
	    $(EMACS) -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: clean
clean: ## Remove build artifacts
	@echo "[clean]" $(BIN)
	@rm -f $(BIN)

.PHONY: test
test:
	@echo "[test] buttercup-run-discover"
	@cask $(EMACS) -batch -f package-initialize -L . -f buttercup-run-discover
