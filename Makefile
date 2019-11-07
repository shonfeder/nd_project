.DEFAULT_GOAL := build

.PHONY: deps
deps:
	-dune build
	opam install . --deps-only --with-test

.PHONY: build
build:
	dune build @default

.PHONY: all
all: deps build

.PHONY: clean
clean:
	dune clean

.PHONY: run
run: build
	dune exec nd_project
