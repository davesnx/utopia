.DEFAULT_GOAL := build

ROOT := ../..
DUNE := opam exec -- dune
COMPILER := $(ROOT)/_build/default/bin/compiler/compiler.exe
CLI := $(ROOT)/_build/default/bin/cli/cli.exe
SERVER := $(ROOT)/_build/default/demo/notes/_utopia/server_main.exe

.PHONY: compile
compile:
	$(DUNE) build ../../bin/compiler/compiler.exe ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
	$(COMPILER) --mode production > /dev/null

.PHONY: build
build: compile
	DUNE_JOBS=$${DUNE_JOBS:-8} $(DUNE) build output.css _utopia/server_main.exe @_utopia/melange @_utopia/esbuild

.PHONY: run
run: build
	$(SERVER)

.PHONY: dev
dev:
	$(DUNE) build ../../bin/cli/cli.exe
	$(CLI) dev

.PHONY: watch-compile
watch-compile:
	$(DUNE) build ../../bin/compiler/compiler.exe
	$(COMPILER) --watch

.PHONY: clean
clean:
	$(DUNE) build ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
