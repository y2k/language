## Why

JavaScript target всегда генерирует runtime и символьные namespace imports с префиксом `./`, поэтому модуль с вложенным `ns`, размещённый в соответствующем вложенном каталоге, не может разрешить зависимости из output root. Это блокирует разбиение приложений на вложенные namespaces, как в GitHub issue #14.

## What Changes

- Считать `ns` каноническим путём JavaScript-модуля относительно output root: `a.b.c` соответствует `a/b/c.js`.
- Вычислять относительный префикс к output root по глубине текущего `ns` и применять его к runtime import и символьным namespace imports.
- Сохранять `./` для корневого namespace и исходника без `ns`.
- Не менять строковые ESM specifiers и Java target.
- Обновить документацию размещения generated JavaScript modules и `language_runtime.js`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: JavaScript imports учитывают глубину текущего namespace при namespace-aligned output layout.

## Impact

- `backend_compiler/js.ml`: вычисление и применение префикса к output root.
- `test/js_ns_test.ml`: проверки корневых и вложенных namespaces, runtime и символьных imports.
- `README.md`: контракт размещения JavaScript modules и runtime.
- Публичная сигнатура `Backend_compiler.Js.compile`, frontend, evaluator, Java target и зависимости не меняются.
