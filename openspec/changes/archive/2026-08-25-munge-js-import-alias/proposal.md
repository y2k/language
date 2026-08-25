## Why

JavaScript compiler передаёт алиас строкового `:require` в ESM import без munging. Алиас с дефисом, например `async-hooks`, создаёт синтаксически недопустимый JavaScript, хотя обращения `async-hooks/member` уже компилируются с тем же алиасом как `async_hooks.member`.

## What Changes

- Применять существующее munging символов к алиасу при генерации ESM import для строкового `:require` в JavaScript target.
- Добавить регрессионную проверку для алиаса с дефисом.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript string requires должны генерировать ESM import с munged локальным алиасом.

## Impact

- Затронут `backend_compiler/js.ml` и `test/js_ns_test.ml`.
- Изменяется только generated JavaScript для string-require алиасов, требующих munging.
- Публичные API и зависимости не изменяются.
