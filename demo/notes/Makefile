.DEFAULT_GOAL := build

ROOT := ../..
DUNE := opam exec -- dune
COMPILER := $(ROOT)/_build/default/bin/compiler/compiler.exe
CLI := $(ROOT)/_build/default/bin/cli/cli.exe
SERVER := $(ROOT)/_build/default/demo/notes/_utopia/server_main.exe

.PHONY: bootstrap-generated-dune
bootstrap-generated-dune:
	@test -f _utopia/dune || { mkdir -p _utopia; : > _utopia/dune; }

.PHONY: compile
compile: bootstrap-generated-dune
	$(DUNE) build --no-print-directory ../../bin/compiler/compiler.exe ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
	$(COMPILER) --mode production > /dev/null

.PHONY: build
build: compile
	DUNE_JOBS=$${DUNE_JOBS:-8} $(DUNE) build --no-print-directory output.css _utopia/server_main.exe @_utopia/melange @_utopia/esbuild

.PHONY: run
run: build
	$(SERVER)

.PHONY: export
export: compile
	DUNE_JOBS=$${DUNE_JOBS:-8} $(DUNE) build --no-print-directory output.css
	$(CLI) export

.PHONY: generate
generate: export

.PHONY: dev
dev: bootstrap-generated-dune
	$(DUNE) build --no-print-directory ../../bin/cli/cli.exe
	$(CLI) dev

.PHONY: watch-compile
watch-compile: bootstrap-generated-dune
	$(DUNE) build --no-print-directory ../../bin/compiler/compiler.exe
	$(COMPILER) --watch

.PHONY: clean
clean: bootstrap-generated-dune
	$(DUNE) build --no-print-directory ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
