## Why

Java compiler теряет `^TYPE` metadata символьных `let` bindings и выводит тип локальной переменной только из Java-типа RHS. Если RHS является функцией языка с return type `Object`, последующий Java method call не компилируется, хотя type hint уже указывает требуемый тип.

## What Changes

- Java compiler будет учитывать `^TYPE` на символьном binding в `let` и приводить RHS к указанному типу перед созданием локальной переменной.
- Добавляется Java-only end-to-end sample, в котором hinted binding получает `Object` из функции языка и используется для Java method call.
- Eval и JavaScript semantics, destructuring bindings и type hints на параметрах функций не меняются.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: Java target поддерживает type hints на символьных `let` bindings.

## Impact

- `backend_compiler/java.ml`: генерация singleton `let*` bindings после lowering.
- `test/samples/java/`: Java-only end-to-end fixture, компилируемый и выполняемый существующим `javac`/`java` harness.
- Новые зависимости, runtime-изменения и публичные API не требуются.
