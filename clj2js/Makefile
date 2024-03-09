deploy: test
	@ cp -f _build/default/bin/main.exe ~/.bin/clj2js

restore:
	opam install . --deps-only -y

test:
	@ clear && dune build && clear && export OCAMLRUNPARAM=b && dune test -f
	@ cd ~/Projects/declarative_ban_bot/.github/ && make test
	@ cd ~/Projects/relax_cats_bot/.github/ && make test
	@ cd ~/Projects/declarative_notify/.github && make test e2e_test

.PHONY: restore test deploy
