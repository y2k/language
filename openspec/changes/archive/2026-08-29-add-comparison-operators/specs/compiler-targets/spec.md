## ADDED Requirements

### Requirement: Compiler target runtimes SHALL provide binary integer comparisons

JavaScript и Java runtimes SHALL предоставлять `_GT_`, `_LT_`, `_GT__EQ_` и `_LT__EQ_` для бинарного сравнения integer значений, представимых соответствующим target.

#### Scenario: Strict integer comparisons
- **WHEN** compiled source вызывает `(> left right)` или `(< left right)` с двумя поддерживаемыми integer значениями
- **THEN** JavaScript и Java target возвращают `true` exactly when `left` соответственно больше или меньше `right`

#### Scenario: Inclusive integer comparisons
- **WHEN** compiled source вызывает `(>= left right)` или `(<= left right)` с двумя поддерживаемыми integer значениями
- **THEN** JavaScript и Java target возвращают `true` exactly when `left` соответственно больше или равен либо меньше или равен `right`

#### Scenario: JavaScript runtime imports
- **WHEN** compiler генерирует JavaScript module
- **THEN** unconditional runtime import предоставляет identifiers `_GT_`, `_LT_`, `_GT__EQ_` и `_LT__EQ_`
