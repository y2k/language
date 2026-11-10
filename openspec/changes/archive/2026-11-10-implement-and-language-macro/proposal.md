# Proposal

## Why

После переноса `cond` следующий небольшой шаг — перенести существующий `and` и проверить внутренние языковые макросы на генерации временных имён и коротком замыкании. Поведение программ должно сохраниться.

## What Changes

- Алгоритм `and` переносится из `frontend/builtin_macros.ml` во встроенный `backend_eval/macros/and.clj`.
- Compile-time окружение получает внутренний helper для существующего `Frontend.Gensym`; пользовательский runtime API не расширяется.
- Сохраняются значения результатов, порядок и однократность вычисления, metadata, автоматическая доступность и работа в зависимостях.
- **BREAKING** для низкоуровневых OCaml callers: `Frontend.parse_and_desugar` и полный pipeline с `extensions:[]` больше не раскрывают `and`. Стандартный `Backend_eval.Pipeline` раскрывает его без импортов и без OCaml fallback, как `cond`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `internal-language-macros`: поставка языкового `and`, временные имена через общий `Gensym` и отключение обоих языковых обработчиков вместе с расширением.

## Impact

`backend_eval/language_macros.ml`, `backend_eval/macros/and.clj`, Dune embedding, `frontend/builtin_macros.ml`; целевые тесты макросов и pipeline, общие samples, проверки зависимостей и metadata. Английский справочник уточняет поставку `and`. Другие макросы, пользовательские `defmacro`, импорты макросов и новые зависимости в объём не входят.
