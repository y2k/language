## ADDED Requirements

### Requirement: JavaScript compiler SHALL compile cast as a transparent expression

JavaScript compiler SHALL compile `(cast TYPE value)` as compiled `value`, не выполняя проверку `TYPE` и не генерируя runtime-зависимость для cast. `value` SHALL вычисляться ровно один раз согласно обычной семантике выражений JavaScript.

#### Scenario: Compile JavaScript cast
- **WHEN** source содержит `(cast java.util.List xs)`
- **THEN** JavaScript output содержит скомпилированное выражение `xs` без вызова или идентификатора `cast`
