# Proposal

## Why

`lower_if` сейчас упаковывает инструкции и результат в expression-`do`. Для дальнейшей поддержки нативного `try` требуется передавать инструкции отдельно от значения, но это меняет общий путь всех value-`if` и должно быть реализовано и проверено самостоятельным изменением.

Пробная правка без согласования `lower_values` нарушила порядок эффектов в существующем `test/samples/if_value.clj`: JS/Java получили `ba` вместо `ab`. Эта правка отменена; proposal описывает будущую согласованную реализацию, а не готовое исправление.

## What Changes

- Возвращать из `lower_if` пару `(statements, result)` вместо упаковки результата в expression-`do`.
- Сохранять вычисление операндов слева направо при переносе prefixes: материализовать ранние вычисляемые значения до инструкций позднего операнда, включая JS lookup callee/метода/конструктора и `this`.
- Доставлять инструкции до существующих тел функций и инициализаторов JS/Java без функции-обёртки ради значения `if`; сохранять границы веток, scopes и отложенных callbacks.
- Выносить результат value-let через temporary, не перемещая consumer и поздние аргументы в его scope. В Java неаннотированный результат сложного выражения может иметь тип Object; для host interop использовать существующие hints на bindings/параметрах или cast. Сохранение inferred типа внутреннего binding за пределами let не гарантируется.
- Сохранить корректные raw-code обращения к локальным именам и выходящий return; не использовать switch-expression. Java static-вызовы гарантируются через Class/method, без распознавания классов в receiver-позиции `.`.
- Проверять изменение на программах без новых форм обработки исключений, включая существующий `if_value.clj`.
- Сделать этот change предварительным условием `add-exception-handling`. Поддержка `try`, `catch`, `finally`, `throw` остаётся в исходном change.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: statement lowering `if` без дополнительных функций и сохранение порядка операндов при извлечении prefixes.

## Impact

`backend_compiler/lowering_expression_to_statement.ml`, потребители lowered forms в `backend_compiler/js.ml` и `backend_compiler/java.ml`; `test/lowering_test.ml`, generation/source-map tests и samples. Совместимость экспортируемого `lower_binding` для FEX должна сохраниться. Новых зависимостей, синтаксиса языка и runtime API нет. Generated source изменится; значения, порядок эффектов, exports и исходные позиции ошибок должны сохраниться.
