SRC   := $(wildcard src/*.el)
ELC   := $(subst .el,.elc,$(SRC))
ELCHKDOC := $(subst .el,.checkdoc,$(SRC))
BIN   := $(ELC) $(ELCHKDOC)
EMACS ?= cask emacs --eval '(add-to-list (quote load-path) (concat default-directory "src/"))'

$(V).SILENT:

.PHONY: ci/cd
ci/cd: lint test

.PHONY: lint
lint: compile checkdoc

.PHONY: checkdoc
checkdoc: $(ELCHKDOC) ## Check for missing or poorly formatted docstrings

%.checkdoc: %.el
	@echo "[checkdoc] $^"
	$(EMACS) -Q --batch \
	    --eval "(or (fboundp 'checkdoc-file) (kill-emacs 1))" \
	    --eval "(setq sentence-end-double-space nil)" \
	    --eval "(checkdoc-file \"$^\")" 2>&1 \
		| tee "$@" \
	    | grep . && exit 1 || true

.PHONY: compile
compile: $(BIN) ## Check for byte-compiler errors

%.elc: %.el
	@echo "[compile] $^"
	if [ -e "$@" ]; then rm -f "$@"; fi
	$(EMACS) -Q --batch -L . -f batch-byte-compile "$^" 2>&1 \
		| grep -v "^Wrote" \
		| grep . && exit 1 || true ;\

.PHONY: clean
clean: ## Remove build artifacts
	@printf "[clean] %s\n" $(BIN)
	rm -f $(BIN)

.PHONY: test
test:
	@echo "[test] buttercup-run-discover"
	$(EMACS) -batch -f package-initialize -L . -f buttercup-run-discover
