.PHONY: test
test: build
	@ ALCOTEST_SHOW_ERRORS=1 dune runtest --profile test

.PHONY: test_smoke
test_smoke: build
	@ ALCOTEST_BAIL=true dune runtest --profile test

.PHONY: build
build:
	@ dune build
	@ test -n "$(LY2K_PACKAGES_DIR)" || { echo "LY2K_PACKAGES_DIR must be set" >&2; exit 1; }
	@ mkdir -p "$(LY2K_PACKAGES_DIR)/prelude/1.0.0/js" "$(LY2K_PACKAGES_DIR)/prelude/1.0.0/java"
	@ cp prelude/language_runtime.js "$(LY2K_PACKAGES_DIR)/prelude/1.0.0/js/language_runtime.js"
	@ cp prelude/language_runtime.java "$(LY2K_PACKAGES_DIR)/prelude/1.0.0/java/language_runtime.java"

.PHONY: clean
clean:
	@ dune clean
