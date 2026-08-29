## 1. Parser semantics

- [x] 1.1 Изменить quoted-string parser в `frontend/parser.ml`, чтобы только `\n` декодировался в один LF, остальные backslash pairs сохраняли текущее поведение; добавить в `test/frontend_desugar_test.ml` точную проверку `SAtom` payload с реальным LF, выполнить `ocamlformat -i frontend/parser.ml test/frontend_desugar_test.ml` и проверить через `dune exec ./test/frontend_desugar_test.exe`.

## 2. Cross-backend regression

- [x] 2.1 Добавить общий fixture в `test/samples/`, сравнивающий `"a\nb"` со строкой с физическим LF, и выполнить `make test`, проверив одинаковый результат на `eval`, JavaScript и Java.
