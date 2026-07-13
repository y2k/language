## ADDED Requirements

### Requirement: Evaluator SHALL treat cast as a transparent core form

Evaluator SHALL обработать `(cast TYPE value)` как core-форму, которая игнорирует `TYPE`, вычисляет `value` ровно один раз и возвращает полученное значение без проверки типа.

#### Scenario: Evaluate cast
- **WHEN** evaluated source содержит `(cast java.util.List (str "a" "b"))`
- **THEN** результатом является `ab`
- **AND** evaluator не пытается разрешить символ `java.util.List`
