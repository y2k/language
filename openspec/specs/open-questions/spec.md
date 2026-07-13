# Open Questions Spec

## Purpose

Track ambiguous observed behavior separately from accepted behavior and proposed changes.

## Requirements

### Requirement: The project SHALL keep unclear current behavior separate from desired changes

This spec records observed ambiguity only. It SHALL NOT be treated as a request to change production behavior.

#### Scenario: Top-level `def` convention versus evaluator support
- **GIVEN** project guidance says `def` is supported only at top level
- **AND** evaluator pattern matching can evaluate `(def ...)` wherever that form is evaluated
- **WHEN** documenting language semantics
- **THEN** nested `def` behavior remains unclear and should not be relied on without an explicit decision

#### Scenario: JavaScript namespace imports ignore Java imports
- **GIVEN** the `ns` macro records both requires and imports
- **WHEN** JavaScript compilation handles `compiler/ns`
- **THEN** require pairs generate imports, while Java import pairs have no documented JavaScript effect

#### Scenario: Type annotations outside Java lambdas
- **GIVEN** parser metadata preserves `^TYPE` annotations
- **AND** Java compiler consumes annotations for lambdas and `gen-class` override methods
- **WHEN** annotations appear on defs, let bindings, or parameters
- **THEN** their intended runtime/compiler effect is unclear beyond being preserved in metadata

#### Scenario: Numeric model
- **GIVEN** parser/compiler number detection accepts floats via `float_of_string_opt`
- **AND** eval arithmetic converts values with `int_of_string_opt`
- **WHEN** decimal numbers are used with eval arithmetic
- **THEN** cross-target numeric behavior is unclear

#### Scenario: Hash map ordering and duplicate keys
- **GIVEN** hash maps are represented as association lists
- **WHEN** maps contain duplicate keys or are stringified
- **THEN** exact duplicate-key semantics and stable ordering expectations are unclear beyond current list behavior

#### Scenario: Java function arity limit
- **GIVEN** Java function interfaces exist for arities 0, 1, and 2
- **WHEN** a top-level Java-compiled function has arity greater than 2
- **THEN** Java compilation fails, but the intended language-level arity limit is unclear

#### Scenario: Parser string escape semantics
- **GIVEN** quoted strings preserve backslash escape pairs textually during parsing
- **WHEN** escape sequences are later emitted or evaluated
- **THEN** the intended language-level escape semantics are unclear beyond current backend string literal handling
