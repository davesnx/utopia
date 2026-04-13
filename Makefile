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
	$(DUNE) build --no-print-directory .

.PHONY: build-prod
build-prod: ## Build for production (--profile=prod)
	$(DUNE) build --no-print-directory .

.PHONY: dev
dev: ## Build in watch mode
	$(DUNE) build --no-print-directory -w @all

.PHONY: clean
clean: ## Clean artifacts
	$(DUNE) clean

.PHONY: test
test: ## Run the unit tests
	$(DUNE) build --no-print-directory @runtest

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	$(DUNE) build --no-print-directory @runtest -w

.PHONY: test-promote
test-promote: ## Updates snapshots and promotes it to correct
	$(DUNE) build --no-print-directory @runtest --auto-promote

.PHONY: deps
deps: $(opam_file) ## Alias to update the opam file and install the needed deps

.PHONY: format
format: ## Format the codebase with ocamlformat
	$(DUNE) build --no-print-directory @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	$(DUNE) build --no-print-directory @fmt

.PHONY: init
setup-githooks: ## Setup githooks
	git config core.hooksPath .githooks

.PHONY: create-switch
create-switch: ## Create opam switch
	opam switch create . 5.4.1 --deps-only --with-dev-setup --with-test --no-install -y

.PHONY: install
install:
	$(DUNE) build --no-print-directory @install
	opam install . --deps-only --with-test --with-dev-setup -y

.PHONY: pin
pin: ## Pin dependencies
	opam pin add server-reason-react.dev "https://github.com/ml-in-barcelona/server-reason-react.git#5be562689712124c1f7344c09f9de4ea9582d13a" -y -n
	opam pin add ochre.dev "https://github.com/davesnx/ochre.git#01f40b92d856064ddb32998defaf96642ae59776" -y -n

.PHONY: init
init: setup-githooks create-switch pin install ## Create a local dev enviroment

.PHONY: bench
bench: ## Run routing micro-benchmarks
	$(DUNE) exec --no-print-directory bench/bench_routing.exe

.PHONY: bench-http
bench-http: ## Run HTTP benchmarks with wrk (requires wrk)
	./bench/bench_http.sh
