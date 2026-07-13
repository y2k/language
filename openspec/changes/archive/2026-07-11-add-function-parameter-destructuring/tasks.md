## 1. Eval Semantics

- [x] 1.1 Обобщить `bind_params` в `backend_eval/eval.ml`, чтобы function parameters использовали те же sequential, associative и nested patterns, что и `let*`.
- [x] 1.2 Привести sequential parameter patterns к `let*`-семантике: absent item связывается с `nil`, extra items игнорируются, при сохранении проверки top-level arity.

## 2. Compiler Lowering

- [x] 2.1 В `backend_compiler/lowering_expression_to_statement.ml` заменять каждый non-symbol `fn*` parameter на fresh identifier из `Gensym`.
- [x] 2.2 Добавлять исходные parameter patterns как initial `let*` bindings и пропускать их через existing lowering, сохраняя порядок body expressions и function arity.
- [x] 2.3 Проверить, что результат lowering передаёт JavaScript и Java generators только symbol parameters и bindings.

## 3. Regression Coverage

- [x] 3.1 Расширить focused eval tests для sequential, associative, nested, missing и extra function parameter values.
- [x] 3.2 Добавить общий sample с public `fn`/`defn` parameter patterns, который выполняется на eval, JavaScript и Java.
- [x] 3.3 Добавить или расширить lowering test для symbol-only параметров и bindings после normalization.

## 4. Verification

- [x] 4.1 Запустить `ocamlformat -i` для изменённых `.ml`/`.mli` файлов.
- [x] 4.2 Запустить `make test_smoke`.
- [x] 4.3 Запустить `make test`.
