project_name = utopia

DUNE = opam exec -- dune
opam_file = $(project_name).opam

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	$(DUNE) build .

.PHONY: build-prod
build-prod: ## Build for production (--profile=prod)
	$(DUNE) build .

.PHONY: dev
dev: ## Build in watch mode
	$(DUNE) build -w @all

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean

.PHONY: test
test: ## Run the unit tests
	$(DUNE) build @runtest

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	$(DUNE) build @runtest -w

.PHONY: test-promote
test-promote: ## Updates snapshots and promotes it to correct
	$(DUNE) build @runtest --auto-promote

.PHONY: deps
deps: $(opam_file) ## Alias to update the opam file and install the needed deps

.PHONY: format
format: ## Format the codebase with ocamlformat
	$(DUNE) build @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	$(DUNE) build @fmt

.PHONY: init
setup-githooks: ## Setup githooks
	git config core.hooksPath .githooks

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.1.1 --deps-only --with-test -y

.PHONY: install
install:
	$(DUNE) build @install
	opam install . --deps-only --with-test

# opam pin add reason-react-ppx.dev "https://github.com/reasonml/reason-react.git#4ee2eda353628090eda95e0b8dabe4e2be50f954" -y
# opam pin add reason-react.dev "https://github.com/reasonml/reason-react.git#4ee2eda353628090eda95e0b8dabe4e2be50f954" -y
# opam pin add melange-fetch.dev "git+https://github.com/melange-community/melange-fetch.git#master" -y
# opam pin add melange-webapi.dev "git+https://github.com/melange-community/melange-webapi.git#master" -y

.PHONY: pin
pin: ## Pin dependencies
	opam pin config.dev "git+https://github.com/ocaml-sys/config.ml" -y
	opam pin libc.dev "git+https://github.com/ocaml-sys/libc.ml" -y
	opam pin rio.dev "git+https://github.com/riot-ml/riot" -y
	opam pin gluon.dev "git+https://github.com/riot-ml/riot" -y
	opam pin add bytestring.dev "https://github.com/riot-ml/riot.git#622c6d67b964a3110a1166fce294f0c6eceeb93a" -y
	opam pin add riot.dev "https://github.com/riot-ml/riot.git#622c6d67b964a3110a1166fce294f0c6eceeb93a" -y
	opam pin add server-reason-react.dev "https://github.com/ml-in-barcelona/server-reason-react.git#68d958d856c87b0b5dd24e7ed400164c206ad56a" -y

.PHONY: init
init: setup-githooks create-switch pin install ## Create a local dev enviroment

.PHONY: lib-test
lib-test: ## Run library tests
	$(DUNE) exec test/test.exe

.PHONY: demo
demo: build ## Run demo executable
	$(DUNE) exec --display-separate-messages --no-print-directory bin/Demo.exe

.PHONY: demo-watch
demo-watch: build ## Run demo executable in watch mode
	$(DUNE) exec --display-separate-messages --no-print-directory bin/Demo.exe --watch

.PHONY: subst
subst: ## Run dune substitute
	$(DUNE) subst

.PHONY: docs
docs: ## Generate odoc documentation
	$(DUNE) build --root . @doc-new

# Because if the hack above, we can't have watch mode
.PHONY: docs-watch
docs-watch: ## Generate odoc docs
	$(DUNE) build --root . -w @doc-new

.PHONY: docs-open
docs-open: ## Open odoc docs with default web browser
	open _build/default/_doc_new/html/docs/local/server-reason-react/index.html

.PHONY: docs-serve
docs-serve: docs docs-open ## Open odoc docs with default web browser
