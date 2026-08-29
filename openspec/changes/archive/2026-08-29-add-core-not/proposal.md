## Why

Вызов core-функции `not` компилируется как обычный function call, но evaluator и target runtimes не предоставляют эту функцию. Сгенерированный JavaScript поэтому успешно создаётся, однако падает при выполнении с `ReferenceError: not is not defined`; eval и Java также не поддерживают тот же вызов.

## What Changes

- Добавить одноаргументную core-функцию `not` в evaluator, JavaScript runtime и Java runtime.
- Определить `not` как логическое отрицание существующей language truthiness: результат равен `true` только для falsey-значений `false` и `nil`, а для остальных значений равен `false`.
- Добавить `not` в существующий unconditional JavaScript runtime import, сохранив generic function-call compilation без отдельной compiler form или frontend macro.
- Проверить одинаковое поведение `not` на `eval`, `js` и `java` одним общим cross-target sample.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: evaluator предоставляет core-функцию `not` с language truthiness semantics.
- `compiler-targets`: JavaScript и Java runtimes предоставляют ту же функцию, а generated JavaScript импортирует её.

## Impact

- `backend_eval/eval_stdlib.ml`: новая stdlib function и binding.
- `backend_compiler/js.ml`: дополнительный identifier в существующем runtime import; snapshots этого import потребуется обновить.
- `test/samples/`: общий regression sample для всех targets.
- Соседний репозиторий `packages`: обратно совместимое обновление `prelude/1.0.0/js/language_runtime.js` и `prelude/1.0.0/java/language_runtime.java`, на которые указывают `prelude/language_runtime.js` и `prelude/language_runtime.java`.
