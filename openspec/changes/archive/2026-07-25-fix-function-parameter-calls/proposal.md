## Why

Java backend генерирует прямой вызов метода для символа в позиции функции, даже когда символ является локальным параметром со значением лямбды. Такой Java-код не компилируется и расходится с корректным поведением eval и JavaScript backends.

## What Changes

- Java backend будет вызывать локальные функциональные значения через соответствующий runtime-интерфейс `Fn0`-`Fn4`.
- Лексическое разрешение локального функционального значения будет иметь приоритет над одноимённой top-level или runtime-функцией.
- Общий sample test проверит передачу лямбды параметром и её вызов на eval, JavaScript и Java targets.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: compiler targets должны поддерживать вызов функционального значения из локального параметра с одинаковой семантикой на JavaScript и Java.

## Impact

Изменения затронут генерацию вызовов в `backend_compiler/java.ml`, delta spec `compiler-targets` и общий sample suite в `test/samples/`. Публичные API и зависимости не меняются.
