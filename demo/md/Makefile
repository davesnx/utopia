.DEFAULT_GOAL := build

ROOT := ../..
DUNE := opam exec -- dune
PYTHON := python3
HOST ?= 127.0.0.1
PORT ?= 8080
COMPILER := $(ROOT)/_build/default/bin/compiler/compiler.exe
CLI := $(ROOT)/_build/default/bin/cli/cli.exe
SERVER := $(ROOT)/_build/default/demo/md/_utopia/server_main.exe
STATIC_DIR := _utopia/static

.PHONY: bootstrap-generated-dune
bootstrap-generated-dune:
	@test -f _utopia/dune || { mkdir -p _utopia; : > _utopia/dune; }

.PHONY: compile
compile: bootstrap-generated-dune
	$(DUNE) build --no-print-directory ../../bin/compiler/compiler.exe ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
	$(COMPILER) > /dev/null

.PHONY: build
build: compile
	DUNE_JOBS=$${DUNE_JOBS:-8} $(DUNE) build --no-print-directory output.css _utopia/server_main.exe

.PHONY: run
run: build
	$(SERVER)

.PHONY: export
export: compile
	DUNE_JOBS=$${DUNE_JOBS:-8} $(DUNE) build --no-print-directory output.css
	$(CLI) export

.PHONY: generate
generate: export

.PHONY: serve
serve: export
	$(PYTHON) -m http.server $(PORT) --bind $(HOST) --directory $(STATIC_DIR)

.PHONY: clean
clean: bootstrap-generated-dune
	$(DUNE) build --no-print-directory ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
