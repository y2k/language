# Proposal

## Why

После `cond`, `and` и `or` нужен последний более сложный перенос перед отдельной работой над архитектурой. `with-open` проверит структурный разбор исходных форм, сохранение metadata и композицию с `let`, Java interop и `try/finally`.

## What Changes

- Перенести алгоритм `with-open` из OCaml во встроенный `backend_eval/macros/with_open.clj`.
- Добавить только необходимые внутренние compile-time операции чтения `Syntax` и сборки списка с metadata исходного узла; исходные дочерние формы остаются невычисленными и сохраняют metadata.
- Сохранить действующую Java-семантику из `frontend-syntax`: последовательное открытие, обратное закрытие, значение тела, аннотации и приоритет ошибок `finally` без suppressed exceptions. Сохранить существующие сообщения об ошибках формы.
- Автоматически поставлять макрос и его helpers в изолированном окружении каждого файла, включая зависимости и executable вне checkout.
- **BREAKING**: frontend-only раскрытие и pipeline с `extensions:[]` больше не раскрывают `with-open`; OCaml fallback удаляется.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `internal-language-macros`: контролируемый доступ к структуре исходных форм и metadata, поставка и отключение языкового `with-open`.

## Impact

`backend_eval/language_macros.ml`, новый языковой исходник и Dune embedding, `frontend/builtin_macros.ml`; прямые тесты макросов, pipeline и зависимостей, перенос проверки раскрытия из frontend-only suite, существующие Java samples и английский справочник. Runtime-семантика из `frontend-syntax` не меняется. Пользовательский `defmacro`, универсальный Syntax API, registry, перестройка bootstrap и расширение поддержки ресурсов на eval/JS остаются за пределами change.
