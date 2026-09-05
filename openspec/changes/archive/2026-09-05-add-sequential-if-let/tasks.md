## 1. Реализация frontend-макроса

- [x] 1.1 Добавить в `frontend/builtin_macros.ml` валидацию и рекурсивное раскрытие `if-let` во вложенные `let*`/`if`, зарегистрировать макрос и проверить сборку командой `dune build`.

## 2. Проверки поведения

- [x] 2.1 Добавить в `test/frontend_desugar_test.ml` проверки точной структуры раскрытия, однократного присутствия RHS, `nil` без `else` и отклонения malformed forms и несимвольных имён; проверить их командой `dune exec ./test/frontend_desugar_test.exe`.
- [x] 2.2 Добавить общий `test/samples/if_let.clj`, покрывающий зависимые bindings, short-circuit обеих ветвей и truthy `0`, и проверить наличие ожидаемого output для каждого сценария в fixture.

## 3. Полная верификация

- [x] 3.1 Отформатировать изменённые `.ml` файлы через `ocamlformat -i`, выполнить `make test` и убедиться, что `if-let` проходит на targets `eval`, `js` и `java` без регрессий.
