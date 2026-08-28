## Context

See proposal.md - Why. `backend_compiler/js.ml` has separate branches for symbolic namespaces and string ESM specifiers. The symbolic branch currently changes only namespace separators, while `Symbol_munge.munge` defines the compiler-wide mapping used for generated JavaScript identifiers.

## Goals / Non-Goals

**Goals:**
- Reuse the existing munging mapping when building a symbolic JavaScript namespace path.
- Preserve the existing transformation of dots into path separators after munging.

**Non-Goals:**
- Не менять `frontend/macro_ns.ml`, Java compiler, evaluator или package resolution.
- Не изменять string ESM specifier-ы, алиасы или правила `Symbol_munge.munge`.

## Decisions

### Munge namespace before rendering its path

Символьный namespace будет сначала проходить через `Symbol_munge.munge`, затем точки будут заменяться на `/`, после чего добавляется существующее расширение `.js`. Порядок даёт `effects-promise.fetch` -> `effects_promise.fetch` -> `effects_promise/fetch` и переиспользует единое отображение символов.

Альтернатива: реализовать отдельную замену `-` на `_` для module path. Она дублирует правила compiler и расходится при будущих дополнениях `Symbol_munge.munge`.

### Сохранить две ветки require

Ветку строкового require не следует пропускать через преобразование namespace: её значение является готовым ESM specifier-ом. Существующие точные compiler tests остаются подходящим уровнем проверки, поскольку контрактом является generated source.

## Risks / Trade-offs

- [Munging преобразует все символы, поддержанные текущей общей таблицей] -> Это согласуется с именами generated package files; тесты фиксируют дефисы в корневом и dotted сегменте.
- [Тест не загружает внешний package] -> Точный import проверяет генератор без сетевой или Node package-зависимости.

## Migration Plan

Миграция и deployment не требуются. Rollback состоит в откате изменения generator и regression tests.
