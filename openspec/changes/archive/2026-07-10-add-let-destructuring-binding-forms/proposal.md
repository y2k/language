## Why

`fn*` уже поддерживает destructuring параметров, но `let`/`let*` принимает только простые имена в binding pairs. Из-за этого одинаковые binding patterns нельзя использовать локально без ручных `get`/`first`-подобных шагов.

## What Changes

- Добавить поддержку destructuring binding forms в `let`: слева от значения можно использовать binding pattern, а не только имя.
- Сохранить существующую последовательную семантику `let*`: значение каждого binding вычисляется в окружении предыдущих binding, затем pattern добавляет новые локальные имена.
- Поддержать те же list/vector-style patterns, которые уже используются для destructuring параметров функций.
- Ошибки несовпадения pattern и значения должны быть одинаковыми по смыслу для eval и compiler targets.
- Полностью устранять destructuring patterns в lowering до передачи кода в JavaScript и Java generators.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `frontend-syntax`: `let` macro должен принимать destructuring binding forms и сохранять их в core `let*`.
- `eval-runtime`: evaluator должен связывать destructuring patterns в `let*`.
- `compiler-targets`: JavaScript и Java targets должны компилировать `let*` с destructuring patterns.

## Impact

- Затронуты `frontend/` macro/desugaring и AST handling для binding forms.
- Затронут `backend_eval/` binding logic для `let*`.
- Затронуты `backend_compiler/lowering_expression_to_statement.ml` и target runtimes для core-функции `get`.
- Нужны focused tests для eval/js/java и при необходимости sample fixture.
