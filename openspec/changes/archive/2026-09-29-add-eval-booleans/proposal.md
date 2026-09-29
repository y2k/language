# Proposal

## Why

Eval представляет boolean и строки одним `Symbol`: строка `"false"` ложна, а `(= false "false")` возвращает `true`. Отдельный boolean устранит это смешение и станет первым шагом к типизированным значениям eval перед regex API из #24.

## What Changes

- Добавить boolean runtime-значение для литералов, цитирования и результатов предикатов.
- **BREAKING**: только `nil` и boolean `false` ложны; `"false"` истинна и не равна `false`.
- Согласовать `if`, `not`, `assert`, сравнения, ключи map, деструктуризацию и передачу boolean через функции и Atom.
- Сохранить вывод `true`/`false` через `str` и CLI, включая boolean внутри коллекций.
- Обновить тесты и английский справочник языка. Числа и строки пока сохраняют существующее представление.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: отдельные boolean, их создание, истинность, равенство и вывод.

## Impact

- Реализация: `backend_eval/eval_types.ml`, `backend_eval/eval.ml`, `backend_eval/eval_stdlib.ml`, `bin/runner.ml`.
- Проверки: `test/eval_ns_test.ml`, общие samples для переносимой истинности строк и eval-only samples для цитирования и типов ключей; `skills/y2k-language/SKILL.md`.
- Новых зависимостей нет; JS/Java runtime и frontend не меняются.
- Основа — уже реализованные `add-eval-nil` и `add-atoms`. Их ещё не синхронизированные deltas нужно учитывать при интеграции, не возвращая старую семантику nil или список bindings без `get-in`.
- Следующий change: `separate-eval-scalar-types`; regex и библиотека Re сюда не входят.
