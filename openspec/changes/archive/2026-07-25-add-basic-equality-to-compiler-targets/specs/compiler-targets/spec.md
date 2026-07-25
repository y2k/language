## ADDED Requirements

### Requirement: Compiler target runtimes SHALL provide basic two-argument equality

JavaScript и Java runtimes SHALL предоставлять `_EQ_` для сравнения ровно двух значений. Для `nil`, boolean, string и integer значений одного типа результат SHALL быть `true`, когда значения равны, и `false`, когда они различаются.

#### Scenario: Equal scalar values
- **WHEN** compiled source вызывает `(= left right)` с двумя равными `nil`, boolean, string или integer значениями одного типа
- **THEN** JavaScript и Java target возвращают `true`

#### Scenario: Unequal scalar values
- **WHEN** compiled source вызывает `(= left right)` с двумя различными boolean, string или integer значениями одного типа
- **THEN** JavaScript и Java target возвращают `false`

#### Scenario: JavaScript runtime import
- **WHEN** JavaScript source содержит вызов `=`
- **THEN** generated runtime import предоставляет identifier `_EQ_`
