## Why

JavaScript compiler формирует путь символьного namespace `:require`, заменяя только точки на `/`. Для namespace с дефисом generated ESM import ссылается на несуществующий файл, хотя package build сохраняет munged basename с `_`.

## What Changes

- Применять существующее `Symbol_munge.munge` к символьному namespace `:require` перед преобразованием точек в сегменты пути JavaScript module specifier.
- Сохранить string `:require` как исходный ESM specifier без munging или добавления относительного пути.
- Добавить regression coverage для namespace с дефисом в корневом и dotted сегменте.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript symbolic namespace requires должны формировать относительные specifier-ы из munged сегментов namespace.

## Impact

- Затронуты `backend_compiler/js.ml` и `test/js_ns_test.ml`.
- Изменяется только generated JavaScript import для символьных namespace с символами, которые преобразует `Symbol_munge.munge`.
- Java target, evaluator, алиасы и string ESM requires не изменяются; новые зависимости не требуются.
