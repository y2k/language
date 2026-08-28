## Why

Вызовы операторов сравнения компилируются в munged identifiers, но evaluator и target runtimes не предоставляют соответствующие функции. В частности, JavaScript для `(> value 0)` генерирует вызов `_GT_`, который завершается `ReferenceError` из-за отсутствующих import и runtime export.

## What Changes

- Добавить бинарные integer-операторы `>`, `<`, `>=` и `<=` в evaluator, JavaScript и Java targets.
- Добавить munged comparison helpers в JavaScript runtime import и в JavaScript/Java runtimes пакета `prelude/1.0.0`.
- Проверить согласованное поведение операторов одним общим cross-target sample.
- Оставить вне контракта wrong arity, non-integer operands, shadowing/redefinition операторов и parity для integer за пределами диапазона конкретного target.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: evaluator предоставляет четыре бинарных integer-оператора сравнения.
- `compiler-targets`: JavaScript и Java runtimes предоставляют те же операторы, а generated JavaScript импортирует соответствующие helpers.

## Impact

- `backend_eval/eval_stdlib.ml`: новые stdlib bindings и вычисление сравнений.
- `backend_compiler/js.ml`: расширение существующего unconditional runtime import.
- `test/samples/`: общий sample для `eval`, `js` и `java`.
- Соседний репозиторий `packages`: обратно совместимое обновление `prelude/1.0.0/js/language_runtime.js` и `prelude/1.0.0/java/language_runtime.java` без создания новой версии package.
