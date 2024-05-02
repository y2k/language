FROM ocaml/opam:ubuntu-20.04-ocaml-5.1 AS build

WORKDIR /app

RUN opam update

COPY clj2js/clj2js.opam .
COPY clj2js/dune-project .
COPY clj2js/bin bin
COPY clj2js/lib lib
COPY clj2js/test test

RUN opam install --deps-only --with-test . \
    eval $(opam env) && \
    dune build

FROM scratch

WORKDIR /app

COPY --from=build /app/_build/default/bin/main.exe clj2js
