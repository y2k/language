## Why

Зависимые проверки optional-значений сейчас требуют вручную вкладывать `let` и `if`, повторяя порядок вычисления и short-circuit control flow. Встроенный `if-let` даст для этого компактную форму с одинаковым поведением в `eval`, JavaScript и Java.

## What Changes

- Добавить встроенный макрос `(if-let [name expression ...] then else?)` с последовательными зависимыми bindings.
- Вычислять каждое binding expression слева направо не более одного раза и прекращать вычисление после первого `false` или `nil`.
- Вычислять `then` только после успешного прохождения всех bindings, а при отсутствии `else` возвращать `nil`.
- Отклонять malformed формы и binding names, не являющиеся symbols.
- Гарантировать bindings только последующим expressions и `then`; программы не должны полагаться на их доступность в `else`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `frontend-syntax`: добавить синтаксис, раскрытие и валидацию последовательного макроса `if-let`.

## Impact

- `frontend/builtin_macros.ml`: новое раскрытие в существующие core-формы `let*`, `if` и `nil`.
- `test/frontend_desugar_test.ml` и общие sample-тесты: проверка структуры, ошибок и поведения всех targets.
- Изменения backend и новые зависимости не требуются.
