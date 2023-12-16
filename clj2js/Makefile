.PHONY: restore test

restore:
	opam install . --deps-only -y

test:
	clear && dune build && clear && export OCAMLRUNPARAM=b && dune test -f
