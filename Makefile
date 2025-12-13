.PHONY: test
test: prelude build
	@ dune build
	@ clear && export OCAMLRUNPARAM=b && cd _build/default/test && ./test.exe -q

.PHONY: test_slow
test_slow: prelude build
	@ dune build
	@ clear && export ALCOTEST_BAIL=true && export OCAMLRUNPARAM=b && dune test -f

.PHONY: build
build: prelude
	@ dune build

.PHONY: deploy
deploy: test_e2e deploy_force

.PHONY: deploy_force
deploy_force: test
	@ echo "\n>>> New ly2k version is deployed <<<\n"
	@ cp -f _build/default/bin/main.exe ~/.local/bin/ly2k

.PHONY: ee_test
ee_test: deploy_force
# @ dune build
	@ rm -rf ~/Projects/finance_tracker/out
# @ cd ~/Projects/finance_tracker && (find . -type f -name "*.clj") | ~/Projects/language/_build/default/bin/main.exe -target eval -src ~/Documents/temp/build.clj > ~/Projects/finance_tracker/Makefile
	@ cd ~/Projects/finance_tracker && ~/Projects/language/_build/default/bin/main.exe -target eval -src ~/Documents/temp/build.clj > ~/Projects/finance_tracker/Makefile

.PHONY: restore
restore:
	@ dune build clj2js.opam
	@ opam install . --deps-only --with-test -y

.PHONY: prelude
prelude:
	@ dune build prelude && OCAMLRUNPARAM=b _build/default/prelude/main.exe

.PHONY: test_e2e
test_e2e: test
	@ cp -f _build/default/bin/main.exe _build/default/bin/clj2js
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/repl test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/interpreter test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/compose_news test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/minesweeper test
	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/finance_tracker && $(MAKE) test
	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/charge_timer && $(MAKE) test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_ban_bot test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/relax_cats_bot test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_notify test e2e_test

.PHONY: docker
docker:
	docker build -f .github/Dockerfile -t y2khub/clj2js .
