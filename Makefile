.PHONY: test
test: prelude
	@ clear && dune build && clear && export OCAMLRUNPARAM=b && dune test -f

.PHONY: deploy
deploy: test_e2e deploy_force

.PHONY: deploy_force
deploy_force: test
	@ cp -f _build/default/bin/main.exe ~/.local/bin/clj2js

.PHONY: restore
restore:
	@ opam install . --deps-only --with-test -y

.PHONY: prelude
prelude:
	@ dune build && OCAMLRUNPARAM=b _build/default/prelude/main.exe

.PHONY: test_e2e
test_e2e: test
	@ cp -f _build/default/bin/main.exe _build/default/bin/clj2js
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/compose_news test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/minesweeper/.github test
	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/finance_tracker && $(MAKE) test
	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/charge_timer && $(MAKE) test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/interpreter test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_ban_bot test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/relax_cats_bot test
	@ PATH=$$PWD/_build/default/bin:$$PATH && $(MAKE) -C ~/Projects/declarative_notify test e2e_test
