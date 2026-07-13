.PHONY: test
test: build
	@ ALCOTEST_SHOW_ERRORS=1 dune runtest --profile test

.PHONY: test_smoke
test_smoke: build
	@ ALCOTEST_BAIL=true dune runtest --profile test

.PHONY: build
build:
	@ dune build

.PHONY: clean
clean:
	@ dune clean
