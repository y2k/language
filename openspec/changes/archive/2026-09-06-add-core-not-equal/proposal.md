## Why

Вызов core-функции `not=` проходит generic function-call compilation и munges в `not_EQ_`, но evaluator и target runtimes не предоставляют соответствующий binding. Из-за этого eval отклоняет символ, generated JavaScript падает с `ReferenceError`, а generated Java не компилируется.

## What Changes

- Добавить `not=` как обычную core-функцию evaluator, JavaScript runtime и Java runtime.
- Для ровно двух scalar-значений одного поддерживаемого типа определить `not=` как логическое отрицание существующей equality operation target.
- Добавить `not_EQ_` в unconditional JavaScript runtime import, сохранив generic function-call compilation.
- Проверить одинаковое поведение на `eval`, `js` и `java` общим execution sample.
- Оставить variadic calls, collections, mixed numeric types и неправильную arity вне гарантированного cross-target контракта.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: stdlib получает core binding `not=` с семантикой отрицания существующего `=`.
- `compiler-targets`: JavaScript и Java runtimes предоставляют `not_EQ_`, а generated JavaScript импортирует этот identifier.

## Impact

- `backend_eval/eval_stdlib.ml`: новая stdlib function и binding.
- `backend_compiler/js.ml`: дополнительный identifier в runtime import и обновление связанных snapshots.
- `prelude/language_runtime.js` и `prelude/language_runtime.java`: additive runtime helper; файлы являются links на versioned `prelude` в соседнем репозитории `packages`.
- `test/samples/`: общий regression sample для evaluator, Node и Java execution.
