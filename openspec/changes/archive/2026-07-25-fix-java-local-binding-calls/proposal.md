## Why

Java generator учитывает параметры функций в `ctx.locals`, но теряет локальные bindings, которые lowering разворачивает в последовательные statement-формы `let*`. Поэтому вызов функционального значения из destructured или обычного `let` binding генерируется как прямой Java method call и либо не компилируется, либо нарушает lexical shadowing.

## What Changes

- Java generator будет последовательно учитывать symbol bindings при компиляции тела функции, lambda и вложенного блока.
- Локальный symbol в позиции функции будет вызываться через существующий runtime-интерфейс `FnN` после любого поддерживаемого способа связывания, включая destructuring.
- Область видимости и порядок bindings сохранятся: binding доступен только следующим формам своего lexical block.
- Общий sample test подтвердит исправление на eval, JavaScript и Java targets.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: явно закрепить вызов функционального значения из последовательного local binding, включая binding, созданный lowering для destructuring.

## Impact

Изменения затронут учёт локального контекста в `backend_compiler/java.ml` и существующий regression sample `test/samples/function_parameter_call.clj`. Публичные API, runtime, lowering и зависимости не меняются.
