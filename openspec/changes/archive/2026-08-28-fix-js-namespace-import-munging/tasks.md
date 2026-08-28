## 1. JavaScript namespace import generation

- [x] 1.1 Apply standard symbol munging to symbolic `:require` namespaces before converting dots to path separators in `backend_compiler/js.ml`, and verify generated imports preserve the required relative `.js` paths.
- [x] 1.2 Extend `test/js_ns_test.ml` with root and dotted hyphenated namespace requires, and verify `dune exec ./test/js_ns_test.exe` passes with `./effect_fetch.js` and `./effects_promise/fetch.js`.

## 2. Regression verification

- [x] 2.1 Run `make test` and verify Java, evaluator, and JavaScript compiler suites remain green.
