# Proposal

## Why

В issue #29 отбор текстовых сообщений и удаление отсутствующих результатов чтения карточек требуют JS `.filter` либо повторения ручного обхода. Нужны переносимые операции prelude, которые соблюдают истинность языка и не теряют `false`, `0` и пустую строку при удалении только отсутствующих результатов.

## What Changes

- Добавить обычные функции `(filter pred items)` и `(keep f items)` на eval, JS и Java для одного list/vector.
- `filter` сохраняет исходные элементы с истинным результатом предиката; `keep` сохраняет результаты преобразования, кроме `nil`. JS `keep` также исключает `undefined`.
- Обход немедленный, слева направо, callback получает один элемент ровно один раз. Порядок сохраняется, операции не изменяют исходную коллекцию.
- Дополнить переносимые sample-тесты и английский справочник языка.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: bindings и семантика `filter`/`keep`, проверки аргументов по образцу `map`.
- `compiler-targets`: доступность и согласованная семантика JS/Java runtime, JS-правило для `undefined`.

## Impact

Затронуты `backend_eval/eval_stdlib.ml`, `prelude/language_runtime.js`, `prelude/language_runtime.java` и runtime import в `backend_compiler/js.ml`. Тесты находятся в `test/samples/` и существующем eval suite; документация — `skills/y2k-language/SKILL.md`. Используется существующая доставка runtime через `Makefile` в `prelude/1.0.0`; новые зависимости, синтаксис и этапы компиляции не требуются. Потребителям нового JS output нужен обновлённый runtime с новыми exports.
