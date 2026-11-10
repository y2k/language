# Proposal

## Why

Подготовительный `prepare-macro-extension-pipeline` завершён: полный путь раскрытия и загрузка пакетов уже поддерживают фабрики расширений. Теперь можно перенести первый существующий макрос `cond` на сам язык, проверив минимальный механизм внутренних макросов на реальных программах.

## What Changes

- Добавить внутреннее непрозрачное значение `Syntax of sexpr` и преобразование результата макроса обратно в AST с сохранением metadata.
- Выполнять встроенное определение `cond.clj` существующим evaluator в отдельном compile-time окружении на каждый исходный файл.
- Встраивать исходник макроса в executable через Dune; включить расширение в default `Backend_eval.Pipeline`.
- Удалить OCaml-алгоритм `cond` без fallback. Адаптер сохраняет проверку полноты пар и прежнее сообщение ошибки.
- Сохранить автоматическую доступность `cond`, его семантику, Java-аннотации и JS source maps. Явный `extensions:[]` отключает языковое расширение, включая `cond`.

Пользовательские `defmacro`, импорты макросов, syntax-quote, другие переносы и новые стадии desugar не входят в этот этап.

## Capabilities

### New Capabilities

- `internal-language-macros`: исполнение поставляемых вместе с компилятором языковых макросов, передача исходных форм, изоляция окружения и композиционное отключение.

### Modified Capabilities

Нет. Требования `frontend-syntax` к результатам `cond`, ошибке неполных пар и metadata сохраняются. Новая capability описывает внутренний контракт выполнения расширения, а не повторяет семантику `cond`.

## Impact

- `backend_eval/eval_types.ml`, `eval_stdlib.ml`, `eval.ml`: внутренний тип и его потребители; новый `language_macros.ml`, `macros/cond.clj`, правило в `backend_eval/dune`.
- `backend_eval/pipeline.ml`: стандартная фабрика; `frontend/builtin_macros.ml`: удаление `cond_macro` и регистрации.
- `test/pipeline_test.ml`: обновление ожиданий пустой конфигурации; отдельные проверки языкового адаптера, `eval_deps_test`, общие samples, Java fixture и JS source maps.
- Низкоуровневый `Frontend.parse_and_desugar` после переноса больше не раскрывает `cond`; полным языковым входом остаётся `Backend_eval.Pipeline`, на который callers уже переведены.
- Новых внешних зависимостей нет. Пользовательский синтаксис и `quote` сохраняются; краткое уточнение английского справочника о встроенном внутреннем макросе не обещает пользовательский API.
- Основание: [решения](../../../wayfinder/cond-mvp/map.md), [spike](../../../wayfinder/cond-mvp/assets/spike-results.md), [завершённая подготовка](../archive/2026-11-10-prepare-macro-extension-pipeline/design.md).
