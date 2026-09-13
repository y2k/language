## Why

Java backend безусловно добавляет `@Override` ко всем методам `gen-class`, поэтому `javac` отклоняет новые instance methods, отсутствующие у предка (issue [#21](https://github.com/y2k/language/issues/21)). Это блокирует в том числе прямой запуск вложенного класса с `public void main()` на JDK 25.

## What Changes

- Разрешить новые `void` instance methods в `gen-class`, полностью убрав генерацию `@Override` для его методов.
- Сохранить переопределения без вызова предка и существующую семантику `^override`: сначала `super.method(...)`, затем helper-реализация.
- Проверить компиляцию и исполнение новых методов, обе разновидности переопределений и прямой запуск instance `main()` на JDK 25.
- Обновить английский справочник языка: новые и переопределяемые `void` instance methods, смысл `^override` и запуск вложенного класса.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: уточнить контракт Java `gen-class` для новых методов, переопределений и instance entry point на JDK 25.

## Impact

- `backend_compiler/java.ml`: генерация методов в `compile_gen_method`; frontend и формат metadata уже достаточны.
- `test/java_ns_test.ml`, Java fixtures и существующий Java test runner: проверки generated source и наблюдаемого исполнения.
- `skills/y2k-language/SKILL.md`: заменить ограничение «Only `void` overrides are supported» точным описанием поведения.
- Новые зависимости, static methods, non-void methods и новая metadata не требуются. Проверка прямого instance entry point требует JDK 25; обычные новые методы не зависят от этой возможности launcher.
