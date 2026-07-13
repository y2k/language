# Project Overview

This repository contains an OCaml implementation of a small Clojure-like language pipeline.

The pipeline is:

1. Parse source text into s-expressions with metadata.
2. Expand built-in macros into core forms.
3. Execute forms with the eval backend, or compile forms to JavaScript or Java.

## Technology

- Language: OCaml.
- Build/test: Dune, Alcotest, `make test`.
- Parser: Angstrom.
- Runtime targets: interpreter (`eval`), JavaScript, Java.
- CLI entrypoint: `dune exec ./bin/main.exe -- --target eval|js|java`.

## Current Behavior Sources

The initial specs describe observed behavior from:

- `frontend/`: AST, parser, macro expansion, namespace macro handling.
- `backend_eval/`: interpreter values, stdlib, namespaces, deps loading.
- `backend_compiler/`: JavaScript and Java code generation plus lowering.
- `bin/runner.ml`: target dispatch and CLI-facing result handling.
- `test/`: focused namespace/deps/compiler tests and sample fixtures.

## Conventions

- `def` is supported at top level by project convention.
- Compiler-generated identifiers must use `Gensym`.
- JavaScript samples run against `language_runtime.js`.
- Java samples compile generated Java with `language_runtime.java`.
- Sample fixtures in `test/samples/*.clj` use the first line as expected output.

## Open Questions

Unclear or under-specified behavior is tracked separately in `openspec/specs/open-questions/spec.md`.
