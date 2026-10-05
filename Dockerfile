FROM ocaml/opam:debian-12-ocaml-5.3 AS builder

USER root
RUN mkdir -p /home/opam/language \
    && chown opam:opam /home/opam/language
COPY --from=node:24-bookworm-slim /usr/local/bin/node /usr/local/bin/node
COPY --from=eclipse-temurin:25-jdk-jammy /opt/java/openjdk /opt/java/openjdk
ENV JAVA_HOME=/opt/java/openjdk
ENV PATH="${JAVA_HOME}/bin:${PATH}"
USER opam
WORKDIR /home/opam/language

RUN opam update \
    && opam install -y 'dune>=3.23' angstrom re.1.13.2 alcotest

COPY --chown=opam:opam dune dune-project Makefile ./
COPY --chown=opam:opam frontend/ frontend/
COPY --chown=opam:opam backend_eval/ backend_eval/
COPY --chown=opam:opam backend_compiler/ backend_compiler/
COPY --chown=opam:opam bin/ bin/
COPY --chown=opam:opam prelude/ prelude/
COPY --chown=opam:opam test/ test/
ENV LANG=C.UTF-8
RUN LY2K_PACKAGES_DIR=/tmp/language-packages opam exec -- make test

FROM debian:bookworm-slim AS runtime
COPY --from=builder /home/opam/language/_build/default/bin/main.exe /usr/local/bin/language
ENTRYPOINT ["/usr/local/bin/language"]
