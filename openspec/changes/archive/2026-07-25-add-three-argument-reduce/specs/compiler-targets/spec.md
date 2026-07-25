## ADDED Requirements

### Requirement: Compiler target runtimes SHALL support reduce with an initial value

JavaScript и Java runtimes SHALL поддерживать `(reduce fn init collection)` для list, выполняя left fold по всем элементам `collection` начиная с `init`. Существующая форма `(reduce fn collection)` SHALL сохранить текущее поведение.

#### Scenario: Reduce a non-empty list with an initial value
- **WHEN** compiled source вызывает `(reduce fn init collection)` с непустым list
- **THEN** runtime вызывает `fn` для каждого элемента по порядку как `fn(acc, item)`, начиная с `init`
- **AND** JavaScript и Java возвращают тот же результат, что и evaluator

#### Scenario: Reduce an empty list with an initial value
- **WHEN** compiled source вызывает `(reduce fn init collection)` с пустым list
- **THEN** runtime возвращает `init`
- **AND** runtime не вызывает `fn`

#### Scenario: Preserve two-argument reduce
- **WHEN** compiled source вызывает `(reduce fn collection)` с непустым list
- **THEN** runtime использует первый элемент как initial accumulator и сворачивает оставшиеся элементы
- **AND** вызов с пустым list продолжает завершаться ошибкой
