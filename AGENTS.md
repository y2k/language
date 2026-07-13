# AGENTS.md

## Commands

- Full verification: `make test` (`dune build`, then `ALCOTEST_SHOW_ERRORS=1 dune runtest --profile test`).
- Faster bail-on-first-failure run: `make test_smoke`.
- Focused OCaml test binary example: `dune exec ./test/java_ns_test.exe`.
- The sample suite `test/test.ml` depends on env vars from `test/dune`; prefer `make test`/`dune runtest` for it.
- Run the CLI with `dune exec ./bin/main.exe -- --target eval|js|java`; input is read from stdin.
- After editing `.ml`/`.mli`, run `ocamlformat -i` on the changed files.

## OpenSpec Language

- When working with OpenSpec (`/opsx-*`, `openspec-*` skills, or files under `openspec/`), answer the user in Russian.
- Write OpenSpec artifacts in Russian, including `proposal.md`, `design.md`, `tasks.md`, and `specs/**/spec.md`; keep code identifiers, file paths, commands, language keywords, and quoted program output unchanged.

## Project Map

- `frontend/`: parser, macro expansion/desugaring, AST, and `Gensym`.
- `backend_eval/`: interpreter backend.
- `backend_compiler/`: JS and Java compilers plus expression-to-statement lowering.
- `bin/runner.ml`: shared CLI/test entrypoint; targets are `eval`, `js`, and `java`.
- `test/java_ns_test.ml`, `test/js_ns_test.ml`, `test/eval_ns_test.ml`, `test/eval_deps_test.ml`: focused Alcotest suites.
- `test/samples/*.clj`: cross-backend fixtures; the first line must be `;; expected-output`.

## Build Gotchas

- Root `dune` turns selected warnings into errors; unused code/imports can break the build.
- `backend_compiler/language_runtime.ml` is generated from `language_runtime.java`; edit `language_runtime.java`, not the generated file.
- JS sample tests write `language_runtime.js` into the test working directory before running Node.
- Java sample tests compile generated Java together with `language_runtime.java` using `javac`.

## Language Semantics

- `def` is supported only at top level. Do not add nested `def` forms or tests that rely on them.
- Use `Gensym.gensym` for generated identifiers in compiler/lowering code; do not add local counters for fresh names.
- Do not emit hardcoded generated temporaries like `__result`, `__tmp`, or any `__XXX` name; generated temporary identifiers must come from `Gensym.gensym`.
- Do not create local `builtin_names`/`string_set` lists for compiler builtins; use a small predicate or shared central definition if builtin detection is needed.
