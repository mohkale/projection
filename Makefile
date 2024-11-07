SRC_DIR = src
BIN_DIR = bin

SRC      := $(wildcard $(SRC_DIR)/*.el $(SRC_DIR)/projection-multi/*.el $(SRC_DIR)/projection-multi-embark/*.el $(SRC_DIR)/projection-dape/*.el)
ELC      := $(patsubst $(SRC_DIR)/%.el,$(BIN_DIR)/%.elc,$(SRC))
ELCHKDOC := $(patsubst $(SRC_DIR)/%.el,$(BIN_DIR)/%.checkdoc,$(SRC))

EMACS ?= cask emacs \
    --eval '(push (concat default-directory "$(SRC_DIR)/") load-path)' \
	--eval '(push (concat default-directory "$(SRC_DIR)/projection-dape") load-path)' \
	--eval '(push (concat default-directory "$(SRC_DIR)/projection-multi") load-path)' \
	--eval '(push (concat default-directory "$(SRC_DIR)/projection-multi-embark") load-path)'

$(V).SILENT:

.PHONY: ci/cd
ci/cd: lint test

.PHONY: lint
lint: compile checkdoc

.PHONY: checkdoc
checkdoc: configure $(ELCHKDOC) ## Check for missing or poorly formatted docstrings

.PHONY: configure
configure: .cask

.cask:
	cask install

$(BIN_DIR)/%.checkdoc: $(SRC_DIR)/%.el
	mkdir -p "$$(dirname "$@")"
	@echo "[checkdoc] $^"
	$(EMACS) -Q --batch \
	    --eval "(or (fboundp 'checkdoc-file) (kill-emacs 1))" \
	    --eval "(setq sentence-end-double-space nil)" \
	    --eval "(checkdoc-file \"$^\")" 2>&1 \
		| sed "s_^$$(basename "$^"):_$^:_" \
		| tee "$@" \
		| grep -E -v -e "\.cask/.*(if|when)-let' is an obsolete macro" \
	    | grep . && exit 1 || true

.PHONY: compile
compile: configure $(ELC) ## Check for byte-compiler errors

$(BIN_DIR)/%.elc: $(SRC_DIR)/%.el
	mkdir -p "$$(dirname "$@")"
	@echo "[compile] $^"
	$(EMACS) -Q --batch \
	    -L . \
	    --eval '(setq create-lockfiles nil)' \
	    -f batch-byte-compile "$^" 2>&1 \
		| grep -v -E -e "^Wrote" -e "^Loading" -e "\.cask/.*(if|when)-let' is an obsolete macro" \
		| grep . && exit 1 || true ;\
	mv -f "$^c" "$@"

.PHONY: clean
clean: ## Remove build artifacts
	for file in $(ELC) $(ELCHKDOC); do \
	    if [ -e "$$file" ]; then \
			echo "[clean] $$file"; \
	        rm -f "$$file"; \
	    fi; \
	done

define run-test
	@echo "[test] buttercup-run-discover $(1)"
	$(EMACS) \
        --eval '(push (concat default-directory "test/lib/") load-path)' \
	    -batch -f package-initialize -L . -f buttercup-run-discover $(1) $(2)
endef

.PHONY: test
test:
	$(call run-test)

.PHONY: test/unit
test/unit:
	$(call run-test,test/unit)

.PHONY: test/integration
test/integration:
	$(call run-test,test/integration)
