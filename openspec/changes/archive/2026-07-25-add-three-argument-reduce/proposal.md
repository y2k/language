## Why

Evaluator уже поддерживает `(reduce fn init collection)`, но JavaScript и Java runtimes предоставляют только двухаргументную форму. Из-за этого одна и та же программа выполняется через `eval`, но завершается ошибкой на compiler targets.

## What Changes

- Добавить в JavaScript runtime поддержку вызова `reduce` с initial value.
- Добавить в Java runtime трёхаргументный overload `reduce`.
- Сохранить существующую семантику `(reduce fn collection)`.
- Проверить обе формы и поведение пустого списка общим cross-backend sample-тестом.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript и Java runtimes будут поддерживать `(reduce fn init collection)` с той же left-fold семантикой, что и evaluator.

## Impact

- `prelude/language_runtime.js`
- `prelude/language_runtime.java`
- `test/samples/`
- `openspec/specs/compiler-targets/spec.md`
- Публичная runtime-функция `reduce` получает дополнительную поддерживаемую арность без breaking changes и новых зависимостей.
