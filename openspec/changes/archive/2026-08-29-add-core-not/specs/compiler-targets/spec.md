## ADDED Requirements

### Requirement: Compiler target runtimes SHALL provide core logical negation

JavaScript и Java runtimes SHALL предоставлять одноаргументную core-функцию `not`, которая возвращает boolean, противоположный language truthiness переданного значения.

#### Scenario: Negate falsey values
- **WHEN** compiled source вызывает `not` со значением `false` или `nil`
- **THEN** JavaScript и Java target возвращают `true`

#### Scenario: Negate truthy values
- **WHEN** compiled source вызывает `not` с любым другим значением, включая integer `0`
- **THEN** JavaScript и Java target возвращают `false`

#### Scenario: JavaScript runtime import
- **WHEN** compiler генерирует JavaScript module
- **THEN** unconditional runtime import предоставляет identifier `not`
