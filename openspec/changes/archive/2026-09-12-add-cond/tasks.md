## 1. Макрос и frontend-проверки

- [x] 1.1 Добавить и зарегистрировать `cond_macro` в `frontend/builtin_macros.ml`: раскрывать пары во вложенные `if` с конечным `nil`, сохранять metadata и отвергать одиночный хвост через `Failure "cond: expected test/result pairs"`; добавить краткий `ponytail:` комментарий о переиспользовании `if`. Дополнить `test/frontend_desugar_test.ml` проверками пустого и многоступенчатого раскрытия, обычного `:else`, metadata и ошибок `(cond true)`, `(cond true 1 false)`, `(cond :else 1 true)`. Выполнить `ocamlformat -i frontend/builtin_macros.ml test/frontend_desugar_test.ml` и проверить успешный `dune exec ./test/frontend_desugar_test.exe`.

## 2. Сквозной контракт

- [x] 2.1 Создать `test/samples/cond.clj` с функцией `test` и ожидаемым выводом в первой строке: включить все семь примеров приёмки issue #18, truthy `0` и журнал через локальный `atom`, подтверждающий порядок и однократность условий, исполнение только выбранного результата и остановку после совпадения. Сохранить выбранное значение до чтения журнала; завершённость подтвердить совпадением вывода sample на `eval`, `js` и `java` при общей проверке в пункте 3.1.

## 3. Общая проверка

- [x] 3.0 Добавить в `backend_compiler/java.ml` обработку `do` в позиции выражения через существующую генерацию аннотированной `Supplier<Object>`-лямбды и вызов `get`. Добавить `test/samples/if_value.clj` для обычного `if` в bindings, аргументах и условиях, порядка эффектов соседних аргументов, пропуска ветвей и top-level initializer. Выполнить `ocamlformat -i backend_compiler/java.ml`; подтвердить исправление прохождением обоих новых samples на трёх targets в `make test`.

- [x] 3.1 Выполнить `make test`; убедиться, что сборка, frontend-проверки и общий sample `cond.clj` проходят на всех трёх targets, а существующие тесты `case`, `if-let` и логических макросов не регрессируют.
