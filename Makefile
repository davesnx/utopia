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
	opam switch create . 5.4.1 --deps-only --with-test -y

.PHONY: install
install:
	$(DUNE) build @install
	opam install . --deps-only --with-test

.PHONY: pin
pin: ## Pin dependencies
	opam pin add server-reason-react.dev "https://github.com/ml-in-barcelona/server-reason-react.git#36ceb5314b6f68b1c666dc6568518111f16d0c83" -y

.PHONY: init
init: setup-githooks create-switch pin install ## Create a local dev enviroment

.PHONY: run-demo
run-demo: build ## Run demo executable
	cd demo/notes && opam exec -- npm run build && ../../_build/default/demo/notes/_utopia/server_main.exe

.PHONY: run-demo-watch
run-demo-watch: build ## Run demo executable in watch mode
	cd demo/notes && ../../_build/default/bin/cli/cli.exe dev

.PHONY: compile-demo
compile-demo: build ## compile demo executable
	cd demo/notes && ../../_build/default/bin/compiler/compiler.exe

.PHONY: compile-demo-watch
compile-demo-watch: build ## compile demo executable in watch mode
	cd demo/notes && ../../_build/default/bin/compiler/compiler.exe --watch

.PHONY: build-generated
build-generated: ## Run generated tests
	cd demo/notes && opam exec -- npm run build

.PHONY: bench
bench: ## Run routing micro-benchmarks
	$(DUNE) exec bench/bench_routing.exe

.PHONY: bench-http
bench-http: ## Run HTTP benchmarks with wrk (requires wrk)
	./bench/bench_http.sh
