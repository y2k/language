FROM ocaml/opam:ubuntu-22.04-ocaml-5.4 AS build

WORKDIR /app

RUN opam update

COPY clj2js.opam .
COPY dune-project .
COPY bin bin
COPY backend backend
COPY core core
COPY macro macro
COPY prelude prelude
COPY stage stage
COPY test test

RUN opam install --deps-only --with-test . && \
    eval $(opam env) && \
    dune build

FROM scratch

WORKDIR /app

COPY --from=build /app/_build/default/bin/main.exe ly2k
