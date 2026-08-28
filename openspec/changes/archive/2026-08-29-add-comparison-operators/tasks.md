## 1. Target Runtimes

- [x] 1.1 Добавить `_GT_`, `_LT_`, `_GT__EQ_` и `_LT__EQ_` в `../packages/prelude/1.0.0/js/language_runtime.js` и проверить их true/false results прямым import через `node --input-type=module`.
- [x] 1.2 Добавить соответствующие binary static methods в `../packages/prelude/1.0.0/java/language_runtime.java` и проверить, что runtime компилируется через `javac`.

## 2. Language Backends

- [x] 2.1 Добавить четыре comparison functions и bindings в `backend_eval/eval_stdlib.ml`, выполнить `ocamlformat -i backend_eval/eval_stdlib.ml` и проверить прямой eval каждого оператора через CLI.
- [x] 2.2 Добавить четыре munged identifiers в unconditional runtime import в `backend_compiler/js.ml`, обновить существующие JS import snapshots, выполнить `ocamlformat -i backend_compiler/js.ml test/js_ns_test.ml` и проверить `dune exec ./test/js_ns_test.exe`.

## 3. Cross-Target Verification

- [x] 3.1 Добавить один общий sample с true и false case для `>`, `<`, `>=` и `<=` и проверить одинаковый результат `eval`, `js` и `java` командой `make test`.
