## Why

Последовательный destructuring параметров функций работает только в eval backend, а JavaScript и Java generators принимают только символьные параметры. Поэтому одна и та же программа с `(fn [[a b]] ...)` выполняется в интерпретаторе, но не компилируется в target backends.

## What Changes

- Поддержать sequential, associative и вложенные destructuring patterns в параметрах `fn`/`fn*` и `defn` на всех targets.
- Привести семантику destructuring параметров к существующей семантике `let*`: отсутствующие list items и hash-map keys связываются с `nil`/`null`, лишние list items игнорируются.
- Normalizing function parameter patterns в compiler lowering до символьных параметров и обычных `let*` bindings, чтобы JavaScript и Java generators не получили отдельную логику destructuring.
- Добавить cross-target coverage публичного синтаксиса `fn` и `defn`, включая nested и associative patterns.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: параметры пользовательских функций принимают те же sequential, associative и nested patterns, что и `let*` bindings.
- `compiler-targets`: lowering устраняет destructuring patterns параметров функций, и JavaScript/Java targets компилируют такие функции.

## Impact

- `backend_eval/eval.ml`: общая binding logic для параметров и `let*`.
- `backend_compiler/lowering_expression_to_statement.ml`: normalization patterns параметров через `Gensym` и `let*`.
- `test/`: тесты eval, JavaScript и Java; возможно общий sample fixture.
- Новый синтаксис, зависимости и специальные ветки в JS/Java codegen не требуются.
