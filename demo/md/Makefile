.DEFAULT_GOAL := build

ROOT := ../..
DUNE := opam exec -- dune
COMPILER := $(ROOT)/_build/default/bin/compiler/compiler.exe
CLI := $(ROOT)/_build/default/bin/cli/cli.exe
SERVER := $(ROOT)/_build/default/demo/md/_utopia/server_main.exe

.PHONY: bootstrap-generated-dune
bootstrap-generated-dune:
	@test -f _utopia/dune || { mkdir -p _utopia; : > _utopia/dune; }

.PHONY: compile
compile: bootstrap-generated-dune
	$(DUNE) build ../../bin/compiler/compiler.exe ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
	$(COMPILER) > /dev/null

.PHONY: build
build: compile
	DUNE_JOBS=$${DUNE_JOBS:-8} $(DUNE) build output.css _utopia/server_main.exe

.PHONY: run
run: build
	$(SERVER)

.PHONY: clean
clean: bootstrap-generated-dune
	$(DUNE) build ../../bin/cli/cli.exe
	$(CLI) clean --build-outputs
