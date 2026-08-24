## Why

JavaScript target преобразует `/` в `.` после генерации любого атома. Поэтому содержимое строковых литералов меняется: `(str "/")` становится `(str)(".")`, из-за чего нельзя передавать URL, пути конфигурации Wrangler и другие значения со слешами.

## What Changes

- Сохранить содержимое строковых литералов без замены `/` при генерации JavaScript.
- Сохранить преобразование `/` в `.` для нестроковых символов с namespace.
- Добавить регрессионную проверку JavaScript output для строки со слешем.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript compiler сохраняет исходное содержимое строковых литералов.

## Impact

- Затрагивает `backend_compiler/js.ml` и `test/js_ns_test.ml`.
- Не меняет публичный синтаксис, runtime-зависимости или Java/eval targets.
