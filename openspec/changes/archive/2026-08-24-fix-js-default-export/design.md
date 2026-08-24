## Context

See `proposal.md` for motivation. `backend_compiler/js.ml` currently compiles every unrecognised list as a function call after symbol munging, so `export-default` becomes the undefined runtime identifier `export_default`. Collection literals have already been desugared to `hash-map` before they reach the JavaScript generator.

## Goals / Non-Goals

**Goals:**

- Emit a static ESM default export for the valid `export-default` form.
- Preserve normal compilation of the exported expression.
- Cover output generation with the existing JavaScript compiler test suite.

**Non-Goals:**

- Изменять evaluator, Java backend, frontend macro expansion или JavaScript runtime.
- Добавлять Cloudflare- или Wrangler-зависимость.
- Проверять количество или расположение форм `export-default`.

## Decisions

### Распознавать `export-default` в JavaScript generator

`backend_compiler/js.ml` получит ветку для `(export-default expression)` до generic function-call ветки. Она скомпилирует `expression` существующим `compile_expr` и сформирует `export default`.

Это минимально меняет только backend, в котором возникает ошибка. Альтернатива с runtime-функцией невозможна: ESM export является синтаксисом модуля, а не операцией времени выполнения. Изменение frontend добавило бы затронутые, но не нужные consumers формы.

### Использовать точный compiler test

`test/js_ns_test.ml` уже сопоставляет generated JavaScript с ожидаемой строкой. Новый тест с объектом `fetch` проверит, что результат содержит `export default` и не является вызовом `export_default`.

Интеграционный тест с Wrangler не нужен: он добавит внешнюю зависимость, а дефект находится в текстовой генерации ESM.

## Risks / Trade-offs

- [Несколько или вложенные формы могут сформировать невалидный ESM] → Проверка не добавляется по согласованному scope; поддерживается генерация valid формы из issue.
- [Точный string test чувствителен к форматированию] → Такой стиль уже используется в `test/js_ns_test.ml` и прямо защищает требуемую генерацию.

## Migration Plan

Изменение не требует миграции. После прохождения compiler test сгенерированные Module Workers будут использовать ESM default export; rollback состоит в откате изменения generator и теста.
