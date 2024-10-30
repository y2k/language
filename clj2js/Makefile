PRELUDE_PATH := $(shell realpath test/samples/prelude/js/src/prelude.clj)
PRELUDE_JAVA_PATH := $(shell realpath test/samples/prelude/java/src/prelude.clj)

test:
	@ clear && dune build && clear && export OCAMLRUNPARAM=b && dune test -f

deploy: test_e2e
	@ cp -f _build/default/bin/main.exe ~/.local/bin/clj2js

restore:
	opam install . --deps-only --with-test -y

test_e2e: test
	@ cp -f _build/default/bin/main.exe _build/default/bin/clj2js
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_ban_bot/ test PRELUDE_PATH=$(PRELUDE_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/relax_cats_bot/.github/ test PRELUDE_PATH=$(PRELUDE_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_notify/.github test e2e_test PRELUDE_PATH=$(PRELUDE_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/minesweeper/.github build PRELUDE_PATH=$(PRELUDE_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/charge_timer build_java build_resources build_dex PRELUDE_JS_PATH=$(PRELUDE_PATH) PRELUDE_JAVA_PATH=$(PRELUDE_JAVA_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/compose_news test PRELUDE_PATH=$(PRELUDE_PATH)

.PHONY: deploy restore test_e2e test
