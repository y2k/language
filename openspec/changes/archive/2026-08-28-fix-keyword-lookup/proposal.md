## Why

Keyword в позиции функции, например `(:TELEGRAM_WEBHOOK_SECRET env)`, сейчас дешугорируется в строку и затем компилируется как вызов строки. Это делает обычный lookup неработоспособным во всех целях, включая JavaScript Cloudflare Worker.

## What Changes

- Дешугорировать двухаргументную форму keyword lookup `(:key collection)` в `(get collection "key")` до преобразования keyword-литерала в строку.
- Сохранить существующее преобразование keyword в строку в map-литералах, destructuring-паттернах и прочих позициях значений.
- Добавить интеграционный sample, проверяющий keyword lookup в `eval`, JavaScript и Java.

## Capabilities

### New Capabilities

- Нет.

### Modified Capabilities

- `frontend-syntax`: keyword в позиции функции с одной коллекцией должен обозначать lookup через core-функцию `get`.

## Impact

- `frontend/builtin_macros.ml`: порядок и правило дешугорирования keyword-вызова.
- `test/samples/`: новый кросс-бэкенд интеграционный sample.
- Публичный синтаксис языка получает работающую форму `(:key collection)`; зависимости и runtime API не меняются.
