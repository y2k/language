# Proposal

## Why

Java-программы сейчас вручную вкладывают `try/finally` для каждого ресурса. `with-open` даст привычную Clojure-форму, сохраняющую порядок закрытия и обработку ошибок без повторения этого шаблона.

## What Changes

- Добавить встроенный макрос `(with-open [name expression ...] body...)` с поддерживаемым контрактом для Java.
- Закрывать ресурсы справа налево через `(cast java.lang.AutoCloseable resource)` и `.close`, включая исключительные выходы.
- Сохранять результат тела и закрывать уже открытые ресурсы при ошибке следующего initializer.
- Описать Java-контракт и добавить frontend- и Java sample-тесты.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `frontend-syntax`: раскрытие `with-open` и семантика управления Java-ресурсами через существующие core-формы.

## Impact

- `frontend/builtin_macros.ml`: встроенный макрос и регистрация.
- `test/frontend_desugar_test.ml`, `test/samples/java/`: проверки раскрытия и исполнения.
- `skills/y2k-language/SKILL.md`, `skills/y2k-language/references/java.md`: английский справочник.
- Используются существующие Java interop, cast и lowering `try/finally`; новые runtime-зависимости не требуются. Поддержка eval, JS и FEX не входит в изменение.
