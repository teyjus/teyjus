.PHONY: default
default: all

.PHONY: all
all:
	@dune build @install --release

.PHONY: install
install:
	@dune build @install --release
	@dune install --release

.PHONY: test
test:
	@dune runtest --release

.PHONY: clean
clean:
	@dune clean
