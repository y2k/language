## 1. Regression Coverage

- [x] 1.1 Расширить `test/frontend_desugar_test.ml` точными проверками reverse patterns в `let` и `fn`, вложенной нормализации и неизменности обычного `{url :url}` map-выражения; запустить `dune exec ./test/frontend_desugar_test.exe` и подтвердить, что новые reverse-сценарии воспроизводят текущую ошибку до исправления.
- [x] 1.2 Добавить один `test/samples/*.clj` fixture с первой строкой expected output, который использует reverse associative destructuring одновременно в `let` и `fn`; проверить, что fixture автоматически входит в общий sample suite.

## 2. Frontend Normalization

- [x] 2.1 Добавить в `frontend/builtin_macros.ml` общий рекурсивный helper для raw brace/bracket binding patterns и вызвать его только для pattern slots в `let_macro` и parameter patterns в `fn_macro`; выполнить `ocamlformat -i frontend/builtin_macros.ml test/frontend_desugar_test.ml` и проверить `dune exec ./test/frontend_desugar_test.exe`.
- [x] 2.2 Запустить `make test` и подтвердить одинаковый результат нового sample на eval, JavaScript и Java, а также отсутствие regressions в существующих key-first destructuring tests.
