# Proposal

## Why

Issue #36 показывает, что общий код приложения с `(eprintln "hello")` не выполняется на eval: отсутствует stdlib binding. `println` также отсутствует, хотя обе функции уже доступны в JavaScript и Java runtime.

## What Changes

- Добавить в eval обычные функции `println` и `eprintln`, доступные без импорта.
- Принимать любое число аргументов, преобразовывать их по правилам `str`, разделять пробелами и завершать строку переводом строки; нулевой вызов печатает пустую строку.
- Направлять вывод соответственно в stdout и stderr и возвращать отдельное nil.
- Проверить разделение потоков, форматирование и результат; описать API и независимый вывод финального значения CLI в справочнике.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: добавить контракт функций построчного вывода и их взаимодействия с существующим финальным результатом runner/CLI.

## Impact

- `backend_eval/eval_stdlib.ml`: native functions и bindings с повторным использованием `to_string`.
- `test/eval_stdlib_contracts_test.ml` и подходящие samples: покрытие новых bindings, возвращаемого значения и потоков CLI.
- `skills/y2k-language/SKILL.md`: описание вывода в eval.
- Новые зависимости не нужны. Генераторы и JS/Java runtime сохраняют существующее поведение.
