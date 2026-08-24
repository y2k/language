## Why

JavaScript backend компилирует `(export-default expression)` как обычный вызов `export_default(...)`. Такой идентификатор не определён, а Cloudflare Module Workers требуют статический ESM `export default` с объектом обработчиков.

## What Changes

- JavaScript target будет компилировать `(export-default expression)` в статическую ESM-декларацию `export default <expression>`.
- Будет добавлен compiler test для default export с объектом `fetch`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript target поддерживает форму `export-default` для генерации ESM default export.

## Impact

- `backend_compiler/js.ml`: генерация JavaScript формы `export-default`.
- `test/js_ns_test.ml`: проверка generated JavaScript default export.
- Публичное поведение `--target js` для Module Workers.
