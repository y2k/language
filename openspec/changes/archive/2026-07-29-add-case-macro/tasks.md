## 1. Реализация макроса

- [x] 1.1 Добавить в `frontend/builtin_macros.ml` раскрытие `case` в `let*` и цепочку `if` с fresh именем от `Gensym.gensym`.
- [x] 1.2 Зарегистрировать `case` в `Builtin_macros.builtin_macros`.

## 2. Проверки поведения

- [x] 2.1 Добавить в `test/frontend_desugar_test.ml` проверки структуры раскрытия, fallback, `nil` без fallback и однократного использования проверяемого выражения.
- [x] 2.2 Добавить sample-тест `case` для совпавшей ветви и fallback на targets `eval`, `js` и `java`.

## 3. Верификация

- [x] 3.1 Отформатировать изменённые `.ml` файлы через `ocamlformat -i`.
- [x] 3.2 Выполнить `make test` и убедиться, что все тесты проходят.
