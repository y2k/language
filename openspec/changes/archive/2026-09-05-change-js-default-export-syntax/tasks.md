## 1. JavaScript generation

- [x] 1.1 Заменить текущую ветку `export-default` в `backend_compiler/js.ml` разбором desugared key/value pairs, validation и генерацией computed properties без `hash_map`; обновить `test/js_ns_test.ml` для нескольких handlers и malformed forms и проверить через `dune exec ./test/js_ns_test.exe`.

## 2. Verification

- [x] 2.1 Выполнить `ocamlformat -i backend_compiler/js.ml test/js_ns_test.ml`, затем запустить `make test` и убедиться, что все targets проходят без изменений frontend и JavaScript runtime.
