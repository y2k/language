## Context

См. `proposal.md` для мотивации. `frontend/macro_ns.ml` уже сохраняет текущий namespace в top-level форме `compiler/ns`, но `backend_compiler/js.ml` отбрасывает его при генерации imports и всегда использует `./`. `Backend_compiler.Js.compile` получает AST и возвращает JavaScript-текст без отдельного имени output-файла.

## Goals / Non-Goals

**Goals:**

- Выводить расположение модуля и глубину его каталога только из текущего `ns`.
- Использовать один префикс к output root для runtime и всех символьных namespace imports.
- Сохранить существующую генерацию для корневых модулей и исходников без `ns`.

**Non-Goals:**

- Не передавать output path или module resolver в `Backend_compiler.Js.compile`.
- Не вычислять кратчайшие пути между namespaces.
- Не проверять соответствие физического output path объявленному `ns`.
- Не менять string requires, frontend, evaluator или Java target.

## Decisions

### Считать `ns` адресом JavaScript-модуля

Namespace `a.b.c` соответствует `a/b/c.js` относительно output root. Для вычисления imports значима глубина каталога, равная количеству сегментов namespace минус один. Исходник без `ns` использует корневую глубину, эквивалентную default namespace `user`.

Альтернатива: передавать фактический output path в compiler API. Она допускает произвольный layout, но расширяет API без необходимости при принятом namespace-aligned контракте.

### Разрешать символьные imports через output root

Compiler вычисляет префикс `./` для корневой глубины либо повторяет `../` для каждого уровня каталога. К этому префиксу добавляется полный путь required namespace после существующего symbol munging и преобразования точек в `/`.

Например, для текущего `app.commands.add` префикс равен `../../`, поэтому `app.commands.remove` импортируется как `../../app/commands/remove.js`.

Альтернатива: вычислять shortest relative path (`./remove.js`). Это требует сравнения namespace-сегментов, тогда как путь через output root уже корректен для родительских, соседних, дочерних и чужих namespaces.

### Не пропускать string requires через namespace resolution

Существующая ветка string require остаётся без изменений: её значение уже является готовым ESM specifier. Вычисленный root prefix применяется только к runtime и символьным namespace imports.

## Risks / Trade-offs

- [Generated module размещён не по пути своего `ns`] → Зафиксировать namespace-aligned layout в `README.md`; проверка layout остаётся ответственностью build tool.
- [Путь через output root длиннее shortest relative path] → Предпочесть единое простое правило, поскольку оба пути корректны для ESM.
- [Регрессия root-level imports] → Сохранить и проверить `./language_runtime.js` и `./<namespace>.js` для корневой глубины.

## Migration Plan

Обновить generator, compiler tests и документацию в одном изменении. Миграция данных и зависимостей не требуется; rollback состоит в откате этих изменений.
