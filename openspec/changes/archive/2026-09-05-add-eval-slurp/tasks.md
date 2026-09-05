## 1. Eval Stdlib

- [x] 1.1 Добавить в `backend_eval/eval_stdlib.ml` одноаргументную функцию `slurp`, которая через стандартный OCaml file API читает файл целиком в `Symbol`, преобразует неверные аргументы в `Eval_error "slurp expects one path"`, ошибки чтения `PATH` в детерминированный `Eval_error "slurp failed: PATH"`, и зарегистрировать binding в `env`; выполнить `ocamlformat -i backend_eval/eval_stdlib.ml` и `dune build`.

## 2. Eval Sample Integration Tests

- [x] 2.1 Минимально изменить eval path в `test/test.ml`, чтобы ожидаемый `Runner.Error` сравнивался с первой строкой sample так же, как успешный результат, сохранив прежнее поведение `js` и `java` paths; выполнить `ocamlformat -i test/test.ml` и проверить сборку через `dune build`.
- [x] 2.2 Добавить ровно три `.clj` fixtures в `test/samples/eval/`: успешное чтение отдельного многострочного text fixture по relative path от текущего рабочего каталога, `slurp` с неверными аргументами и отсутствующий файл; не добавлять тесты в `test/eval_ns_test.ml` или common samples и выполнить `make test`.
