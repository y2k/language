## 1. JavaScript Import Resolution

- [x] 1.1 Обновить `backend_compiler/js.ml` и `test/js_ns_test.ml`: вычислять prefix к output root из текущего `ns`, применять его только к runtime и символьным namespace imports, покрыть корневую и вложенную глубину, родительские, соседние, дочерние и чужие namespaces и проверить `ocamlformat -i backend_compiler/js.ml test/js_ns_test.ml` и `dune exec ./test/js_ns_test.exe`.

## 2. Documentation And Verification

- [x] 2.1 Обновить `README.md`, описав namespace-aligned JavaScript output layout и размещение `language_runtime.js` в output root, и проверить соответствие примеров delta spec.
- [x] 2.2 Запустить `make test` и подтвердить отсутствие regressions во всех targets.
