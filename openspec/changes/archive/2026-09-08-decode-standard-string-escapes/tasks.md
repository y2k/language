## 1. Декодирование frontend

- [x] 1.1 Расширить escape-match в `frontend/parser.ml` для `\"`, `\\`, `\t`, `\r`, сохранив `\n`, однократность и fallback неизвестных пар; добавить краткий `ponytail:` комментарий о намеренной границе. В `test/frontend_desugar_test.ml` проверить точные независимо заданные payload для пяти escapes, смешанной строки, `"\\n"`, неизвестных пар и HTML из #13. Отформатировать изменённые `.ml` через `ocamlformat -i` и выполнить `dune exec ./test/frontend_desugar_test.exe`.

## 2. Сериализация JavaScript

- [x] 2.1 В строковой ветке `compiler/ns` в `backend_compiler/js.ml` сериализовать payload существующим `js_string`, сохранив alias munging и символьные requires. Расширить `test/js_ns_test.ml` независимыми expected-source проверками обычных строк и string requires: кавычка/HTML, обратный слеш, LF/TAB/CR, отсутствие повторного декодирования и неизвестная пара. Отформатировать изменённые `.ml` через `ocamlformat -i` и выполнить `dune exec ./test/js_ns_test.exe`, включая существующие namespace regressions.

## 3. Сквозная проверка

- [x] 3.1 Добавить `test/samples/string_standard_escapes.clj` с однострочным ожидаемым выводом HTML из #13, одиночного обратного слеша и буквального `\n`, используя существующий sample harness без новых зависимостей. Проверить фактический вывод на eval/JS/Java через `make test`; сохранить прохождение существующего `string_newline_escape.clj` и всех focused suites.
