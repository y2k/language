# Proposal

## Why

В Forum Filter обход найденных тредов ради DOM-кликов сейчас требует JavaScript `.forEach`. Issue #26 предлагает выразить этот эффект через `(run! f items)`, не собирая ненужные результаты `map` и не передавая искусственный аккумулятор `reduce`.

## What Changes

- Добавить обычную функцию `run!` на eval, JavaScript и Java: немедленный последовательный обход list/vector, один вызов unary callback на элемент, игнорирование результатов и возврат настоящего `nil` после успешного обхода.
- Пустая коллекция не вызывает callback; ошибки callback прерывают обход без отката уже выполненных эффектов.
- Поддержать обычные JS-массивы, включая полученные из DOM через `Array.from`. Непосредственный обход `NodeList`, maps, `nil` как коллекция и произвольные iterable не входят в контракт.
- Добавить переносимые sample-проверки, JS-сценарий DOM-подобных объектов и обновить английский справочник языка.
- Устранить выявленные namespace-препятствия: Java references именованных функций должны указывать текущий класс, JS sample-runner должен исполнять namespace-модуль по соответствующему пути.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: binding `run!`, семантика обхода и проверки аргументов по существующему образцу `map`.
- `compiler-targets`: доступность `run!` через runtime JS/Java и переносимый контракт эффектов, включая обычный JS-массив host-объектов.

## Impact

- `backend_eval/eval_stdlib.ml`, `prelude/language_runtime.js`, `prelude/language_runtime.java`, runtime import в `backend_compiler/js.ml`.
- `backend_compiler/java.ml` для имени класса в method references и `test/test.ml` для размещения JS sample-модулей; отдельный переносимый regression fixture для namespace callback.
- Общие fixtures в `test/samples/`, JS-specific fixture в `test/samples/js/`, целевые eval-тесты для ошибок callback и аргументов.
- `skills/y2k-language/SKILL.md`: описание нового API и границ переносимости.
- Без новых зависимостей, макросов, форм AST или изменений lowering. Существующие `map` и `reduce` не меняются.
