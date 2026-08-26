## 1. Frontend metadata and macros

- [x] 1.1 Добавить `private_` с default `false` в `Frontend.meta` и parser metadata; проверить `dune build`.
- [x] 1.2 Реализовать повторное outer macro expansion до отсутствия matching macro и сохранить metadata replacement forms; добавить frontend regression tests для цепочки `defn-` -> `defn` -> `def` -> `fn*` и выполнить `dune exec ./test/frontend_desugar_test.exe`.
- [x] 1.3 Добавить macro expansions `def-` -> private `def` и `defn-` -> private `defn`; проверить в frontend tests private metadata у итогового core `def`.

## 2. JavaScript generation

- [x] 2.1 Генерировать `export const` для public top-level `def` и `const` для private `def` в `backend_compiler/js.ml`; выполнить `dune exec ./test/js_ns_test.exe`.
- [x] 2.2 Обновить и добавить точные JS output tests для `defn`, `defn-`, `def` и `def-`, включая отсутствие `def_`; выполнить `dune exec ./test/js_ns_test.exe`.

## 3. Verification

- [x] 3.1 Запустить `ocamlformat -i` для изменённых `.ml`/`.mli` файлов и проверить `make test`.
