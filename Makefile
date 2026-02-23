.PHONY: test
test: build
	@ dune build
	@ clear && export ALCOTEST_BAIL=true && export OCAMLRUNPARAM=b && dune test -f

.PHONY: test_smoke
test_smoke: build
	@ dune build
	@ clear && export OCAMLRUNPARAM=b && cd _build/default/test && ./test.exe -q

.PHONY: build
build:
	@ dune build

.PHONY: restore
restore:
	@ dune build clj2js.opam
	@ opam install . --deps-only --with-test -y

.PHONY: test_e2e
test_e2e: test
	@ $(MAKE) -C ~/Projects/declarative_ban_bot clean test
	@ $(MAKE) -C ~/Projects/interpreter clean test
# 	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/repl test
# 	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/compose_news test
# 	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/minesweeper test
# 	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/finance_tracker && $(MAKE) test
# 	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/charge_timer && $(MAKE) test
# 	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/relax_cats_bot test
# 	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_notify test e2e_test

.PHONY: docker
docker:
	docker build -f .github/Dockerfile -t y2khub/clj2js .
