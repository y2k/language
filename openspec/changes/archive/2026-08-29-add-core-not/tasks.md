## 1. Target Runtimes

- [x] 1.1 Добавить `not` в `../packages/prelude/1.0.0/js/language_runtime.js` как отрицание существующей `truthy` semantics и проверить `false`, `nil`/`null`, `0` и truthy value прямым import через `node --input-type=module`.
- [x] 1.2 Добавить соответствующий одноаргументный static method в `../packages/prelude/1.0.0/java/language_runtime.java` и проверить, что runtime компилируется командой `javac`.

## 2. Language Backends

- [x] 2.1 Добавить функцию `not` и stdlib binding в `backend_eval/eval_stdlib.ml`, выполнить `ocamlformat -i backend_eval/eval_stdlib.ml` и проверить результаты для `false`, `nil`, `0` и truthy value через CLI с `--target eval`.
- [x] 2.2 Добавить `not` в unconditional runtime import в `backend_compiler/js.ml`, обновить существующие snapshots import, выполнить `ocamlformat -i backend_compiler/js.ml test/js_ns_test.ml` и проверить `dune exec ./test/js_ns_test.exe`.

## 3. Cross-Target Verification

- [x] 3.1 Добавить один общий fixture в `test/samples/`, покрывающий falsey values, truthy `0` и `(not (= value 0))`, и проверить одинаковый результат `eval`, `js` и `java` командой `make test`.
