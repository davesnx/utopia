.DEFAULT_GOAL := build

ROOT := ../..
DUNE := opam exec -- dune
NODE := node
PYTHON := python3
HOST ?= 127.0.0.1
PORT ?= 8080
COMPILER := $(ROOT)/_build/default/bin/compiler/compiler.exe
CLI := $(ROOT)/_build/default/bin/cli/cli.exe
SERVER := $(ROOT)/_build/default/demo/blog/_utopia/server_main.exe
STATIC_DIR := _utopia/static

.PHONY: compile
compile:
	$(DUNE) build ../../bin/compiler/compiler.exe ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
	$(COMPILER) > /dev/null

.PHONY: build
build: compile
	DUNE_JOBS=$${DUNE_JOBS:-8} $(DUNE) build output.css _utopia/server_main.exe

.PHONY: run
run: build
	$(SERVER)

.PHONY: generate
generate: build
	$(SERVER) --ssg

.PHONY: serve
serve: generate
	$(PYTHON) -m http.server $(PORT) --bind $(HOST) --directory $(STATIC_DIR)

.PHONY: clean
clean:
	$(DUNE) build ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
