.DEFAULT_GOAL := build

.PHONY: deps
deps:
	-dune build
	opam install . --deps-only --with-test

.PHONY: build
build:
	dune build @default

.PHONY: watch
watch:
	dune build -w

.PHONY: all
all: deps build

.PHONY: clean
clean:
	dune clean

.PHONY: view
view: build
	xdg-open _build/default/frontend/index.html
	dune build @default {public/*,frontend/index.html} -w

.PHONY: test
test:
	dune runtest
