## 1. Runtime Support

- [x] 1.1 Добавить evaluator function и stdlib binding `not=`, переиспользующие существующий `equal`, и проверить через `dune exec ./bin/main.exe -- --target eval`, что равные scalar-аргументы дают `false`, а различные дают `true`.
- [x] 1.2 Добавить `not_EQ_` в `prelude/language_runtime.js` и `prelude/language_runtime.java` как отрицание существующего `_EQ_` и проверить прямыми runtime-вызовами результаты для равных и различных scalar-значений.
- [x] 1.3 Добавить `not_EQ_` в unconditional import в `backend_compiler/js.ml`, обновить snapshots в `test/js_ns_test.ml`, выполнить `ocamlformat -i` для изменённых `.ml` файлов и проверить snapshots командой `dune exec ./test/js_ns_test.exe`.

## 2. Cross-Target Regression

- [x] 2.1 Добавить common sample в `test/samples/` для равных и различных `nil`, boolean, string и integer аргументов `not=` и проверить его исполнение через `eval`, Node и Java командой `make test`.
