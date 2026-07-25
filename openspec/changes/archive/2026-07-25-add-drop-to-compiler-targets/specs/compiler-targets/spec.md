## ADDED Requirements

### Requirement: Compiler target runtimes SHALL provide core drop

JavaScript и Java runtimes SHALL предоставлять core-функцию `drop`, которая принимает целое число и list и возвращает list без первых указанного количества элементов.

#### Scenario: Drop items from a list
- **WHEN** compiled source вызывает `(drop n items)` с положительным целым `n`, меньшим длины `items`
- **THEN** target runtime возвращает оставшиеся элементы в исходном порядке
- **AND** JavaScript и Java возвращают тот же результат, что и evaluator

#### Scenario: Drop with boundary counts
- **WHEN** `n` меньше или равно нулю
- **THEN** target runtime возвращает все элементы list
- **WHEN** `n` не меньше длины list
- **THEN** target runtime возвращает пустой list
