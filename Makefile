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
	opam switch create . 5.4.1 --deps-only --with-dev-setup --with-test --no-install -y

.PHONY: install
install:
	$(DUNE) build @install
	opam install . --deps-only --with-test --with-dev-setup -y

.PHONY: pin
pin: ## Pin dependencies
	opam pin add server-reason-react.dev "https://github.com/ml-in-barcelona/server-reason-react.git#5be562689712124c1f7344c09f9de4ea9582d13a" -y -n
	opam pin add ochre.dev "https://github.com/davesnx/ochre.git#054c4973ee2ea475956ae374a63ded3c0768e467" -y -n

.PHONY: init
init: setup-githooks create-switch pin install ## Create a local dev enviroment

.PHONY: run-demo
run-demo: ## Run demo executable
	$(MAKE) -C demo/notes run

.PHONY: run-demo-watch
run-demo-watch: ## Run demo executable in watch mode
	$(MAKE) -C demo/notes dev

.PHONY: compile-demo
compile-demo: ## compile demo executable
	$(MAKE) -C demo/notes compile

.PHONY: compile-demo-watch
compile-demo-watch: ## compile demo executable in watch mode
	$(MAKE) -C demo/notes watch-compile

.PHONY: build-generated
build-generated: ## Run generated tests
	$(MAKE) -C demo/notes build

.PHONY: bench
bench: ## Run routing micro-benchmarks
	$(DUNE) exec bench/bench_routing.exe

.PHONY: compile-blog
compile-blog: ## Compile blog demo
	$(MAKE) -C demo/blog compile

.PHONY: run-blog
run-blog: ## Run blog demo
	$(MAKE) -C demo/blog run

.PHONY: generate-blog
generate-blog: ## Generate static blog pages
	$(MAKE) -C demo/blog generate

.PHONY: compile-md
compile-md: ## Compile markdown demo
	$(MAKE) -C demo/md compile

.PHONY: run-md
run-md: ## Run markdown demo
	$(MAKE) -C demo/md run

.PHONY: bench-http
bench-http: ## Run HTTP benchmarks with wrk (requires wrk)
	./bench/bench_http.sh
