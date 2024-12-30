FROM ocaml/opam:ubuntu-20.04-ocaml-5.1 AS build

WORKDIR /app

RUN opam update

COPY clj2js.opam .
COPY dune-project .
COPY bin bin
COPY lib lib
COPY test test

RUN opam install --deps-only --with-test . && \
    eval $(opam env) && \
    dune build

FROM scratch

WORKDIR /app

COPY --from=build /app/_build/default/bin/main.exe clj2js
