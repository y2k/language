## ADDED Requirements

### Requirement: Compiler target runtimes SHALL provide basic two-argument inequality

JavaScript и Java runtimes SHALL предоставлять `not_EQ_` для сравнения ровно двух значений. Для поддерживаемых `nil`, boolean, string и integer значений результат SHALL быть логическим отрицанием результата существующей `_EQ_` operation target для тех же аргументов.

#### Scenario: Equal scalar values are not unequal
- **WHEN** compiled source вызывает `(not= left right)` с двумя равными `nil`, boolean, string или integer значениями одного типа
- **THEN** JavaScript и Java target возвращают `false`

#### Scenario: Unequal scalar values are unequal
- **WHEN** compiled source вызывает `(not= left right)` с двумя различными boolean, string или integer значениями одного типа
- **THEN** JavaScript и Java target возвращают `true`

#### Scenario: JavaScript runtime import
- **WHEN** compiler генерирует JavaScript module
- **THEN** unconditional runtime import предоставляет identifier `not_EQ_`
