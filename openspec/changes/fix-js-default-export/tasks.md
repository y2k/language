## 1. Генерация ESM default export

- [ ] 1.1 Добавить в `backend_compiler/js.ml` генерацию `export default` для `(export-default expression)` до generic function-call ветки; проверить вывод CLI для примера из issue №2.

## 2. Регрессия

- [ ] 2.1 Добавить в `test/js_ns_test.ml` точную проверку generated JavaScript для `export-default` с объектом `fetch`; проверить `dune exec ./test/js_ns_test.exe`.
- [ ] 2.2 Запустить `make test` и убедиться, что все suites проходят.
