PRELUDE_PATH := $(shell realpath test/samples/prelude/js/src/prelude.clj)
PRELUDE_JAVA_PATH := $(shell realpath test/samples/prelude/java/src/prelude.clj)
PRELUDE_JAVA_DIR:= $(shell dirname $(PRELUDE_JAVA_PATH))

.PHONY: test
test:
	@ clear && dune build && clear && export OCAMLRUNPARAM=b && dune test -f

.PHONY: deploy
deploy: test_e2e
	@ cp -f _build/default/bin/main.exe ~/.local/bin/clj2js

.PHONY: restore
restore:
	@ opam install . --deps-only --with-test -y

.PHONY: test_e2e
test_e2e: test
	@ cp -f _build/default/bin/main.exe _build/default/bin/clj2js
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/interpreter test PRELUDE_PATH=$(PRELUDE_JAVA_DIR)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/finance_tracker build_java test PRELUDE_PATH=$(PRELUDE_PATH) ANDROID_PRELUDE_PATH=$(PRELUDE_JAVA_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/charge_timer build_java build_resources build_dex PRELUDE_JS_PATH=$(PRELUDE_PATH) PRELUDE_JAVA_PATH=$(PRELUDE_JAVA_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_ban_bot/ test PRELUDE_PATH=$(PRELUDE_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/relax_cats_bot/.github/ test PRELUDE_PATH=$(PRELUDE_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_notify/.github test e2e_test PRELUDE_PATH=$(PRELUDE_PATH)
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/minesweeper/.github build PRELUDE_PATH=$(PRELUDE_PATH)
# @ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/compose_news test PRELUDE_PATH=$(PRELUDE_PATH)
