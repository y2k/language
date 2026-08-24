## Why

JavaScript compiler превращает строковые `:require` для Node built-ins и npm-пакетов в относительные пути с расширением `.js`. Полученный ESM-код синтаксически некорректен и не позволяет использовать внешние модули, например `"node:test"` и `"wrangler"`.

## What Changes

- JavaScript target будет сохранять строковый module specifier из `:require` и генерировать для него bare ESM import.
- Символьные namespace require сохранят текущую генерацию относительного пути `./path/to/namespace.js`.
- Будут добавлены compiler tests для строкового и символьного require.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript namespace requires различают строковые ESM module specifier и символьные локальные namespace paths.

## Impact

- `backend_compiler/js.ml`: генерация import для `compiler/ns`.
- `test/js_ns_test.ml`: проверки generated JavaScript imports.
- Публичное поведение `--target js` для форм `(ns ... (:require ...))`.
