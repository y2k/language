deploy: test_e2e
	@ cp -f _build/default/bin/main.exe ~/.bin/clj2js

restore:
	opam install . --deps-only -y

test_e2e: test
	@ cp -f _build/default/bin/main.exe _build/default/bin/clj2js
	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/declarative_ban_bot/.github/ && make test
	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/relax_cats_bot/.github/ && make test
	@ PATH=$$PWD/_build/default/bin:$$PATH && cd ~/Projects/declarative_notify/.github && make test e2e_test

test:
	@ clear && dune build && clear && export OCAMLRUNPARAM=b && dune test -f

.PHONY: deploy restore test_e2e test
