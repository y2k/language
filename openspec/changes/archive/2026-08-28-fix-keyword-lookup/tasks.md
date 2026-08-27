## 1. Frontend

- [x] 1.1 Добавить macro для `(:key collection)`, раскрывающий форму в `(get collection "key")` до `keyword_macro`, и проверить результат focused frontend test.
- [x] 1.2 Добавить focused test, подтверждающий, что keyword-вызов раскрывается в `get`, а keyword в map-литерале остаётся строковым ключом; проверить `dune exec ./test/frontend_desugar_test.exe`.

## 2. Интеграция

- [x] 2.1 Добавить кросс-бэкенд sample keyword lookup с map, который выполняется через существующий `eval`/JS/Java harness, и проверить `make test`.
