deploy: test
	cp -f _build/default/bin/main.exe ~/.bin/clj2js

restore:
	opam install . --deps-only -y

test:
	clear && dune build && clear && export OCAMLRUNPARAM=b && dune test -f

.PHONY: restore test deploy
