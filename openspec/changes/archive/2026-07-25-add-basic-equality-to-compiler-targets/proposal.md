## Why

Символ `=` компилируется в `_EQ_`, но JavaScript и Java runtimes не предоставляют такую функцию, поэтому корректно сгенерированные вызовы сравнения не выполняются. Первой итерации достаточно для сравнения ровно двух базовых scalar-значений без определения полной структурной семантики.

## What Changes

- Добавить `_EQ_` с двумя аргументами в JavaScript runtime на основе `Object.is`.
- Добавить `_EQ_` с двумя аргументами в Java runtime на основе `Objects.equals`.
- Импортировать `_EQ_` в сгенерированный JavaScript.
- Проверить равные и неравные `nil`, boolean, string и integer значения на compiler targets.
- Оставить collections, functions, mixed numeric types и variadic equality вне scope этой итерации.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript и Java runtimes поддерживают двухаргументный `=` для базовых scalar-значений.

## Impact

Изменение затрагивает JavaScript generator, versioned JavaScript и Java runtime-файлы пакета `prelude` в связанном packages repo, compiler snapshots и cross-backend sample-тесты. Новые зависимости не требуются.
