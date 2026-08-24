## 1. JavaScript import generation

- [x] 1.1 Обновить генерацию `compiler/ns` в `backend_compiler/js.ml`, чтобы строковый module specifier сохранялся в bare ESM import, а символьный namespace оставался относительным путём; проверить `dune exec ./test/js_ns_test.exe`.

## 2. Regression coverage

- [x] 2.1 Добавить в `test/js_ns_test.ml` проверку строкового `:require` для `"node:test"` или `"wrangler"` и сохранить проверку символьного local require; проверить `dune exec ./test/js_ns_test.exe`.
- [x] 2.2 Запустить полный набор проверок `make test` и убедиться, что все target suites проходят.
