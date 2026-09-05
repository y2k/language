## Why

Текущий `(export-default expression)` сохраняет обычную компиляцию map literal через `hash_map`, который создаёт JavaScript-значение с null prototype. Cloudflare Module Workers отклоняют такой default export с ошибкой 10021, поэтому форма должна создавать обычный handler object напрямую.

## What Changes

- **BREAKING** Заменить `(export-default expression)` на `(export-default key value ...)` с одной или несколькими парами обработчиков; key может быть keyword или string literal.
- Компилировать пары в статический ESM `export default` с обычным JavaScript object literal, не вызывая `hash_map` для экспортируемого объекта.
- Отклонять пустую форму, нечётное число аргументов и ключи, которые после общего desugaring не являются строковыми atoms.
- Сохранить текущую null-prototype семантику всех обычных language hash maps.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: изменить публичный синтаксис и JavaScript output формы `export-default` для Cloudflare Module Worker handler objects.

## Impact

- `backend_compiler/js.ml`: разбор desugared пар `export-default`, validation и генерация обычного JavaScript object literal.
- `test/js_ns_test.ml`: проверки malformed forms и generated JavaScript.
- `openspec/specs/compiler-targets/spec.md`: при синхронизации будет заменён существующий сценарий Default export.
- Пользователи старого `(export-default expression)` должны переписать source на пары `keyword value`; новые зависимости и изменения runtime не требуются.
