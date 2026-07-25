## Why

Функция `drop` доступна в evaluator, но скомпилированные JavaScript и Java программы не могут её вызвать. Это нарушает единообразие поведения общей stdlib между тремя платформами.

## What Changes

- Добавить `drop` в JavaScript runtime и импортировать её в сгенерированный JavaScript.
- Добавить `drop` в Java runtime.
- Проверить единое поведение `drop` на eval, JavaScript и Java общим sample-тестом.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript и Java runtimes предоставляют `drop` с тем же поведением для списков, что и evaluator.

## Impact

Изменение затрагивает `backend_compiler/js.ml`, runtime-файлы пакета `prelude` из связанного packages repo, ожидаемые JavaScript compiler-тесты и общий набор sample-тестов. Новые зависимости не требуются.
