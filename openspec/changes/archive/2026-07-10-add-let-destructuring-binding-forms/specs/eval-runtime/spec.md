## MODIFIED Requirements

### Requirement: The evaluator SHALL support core control and binding forms

The evaluator SHALL support top-level `def`, `compiler/ns`, `deps`, `quote`, `if`, `fn*`, `let*`, and `do` forms. `let*` bindings SHALL accept symbol bindings, sequential destructuring patterns, and associative destructuring patterns.

#### Scenario: Define in current namespace
- **WHEN** top-level `(def name value)` is evaluated
- **THEN** `value` is evaluated and stored as `name` in the current namespace globals

#### Scenario: Compiler namespace form
- **WHEN** `(compiler/ns "app" (("dep.ns" "alias")) ())` is evaluated
- **THEN** the current namespace becomes `app`
- **AND** alias `alias` resolves qualified lookups to namespace `dep.ns`

#### Scenario: Quote form
- **WHEN** `(quote value)` is evaluated
- **THEN** atoms become symbol values and lists become runtime lists without evaluating their contents

#### Scenario: If without else
- **WHEN** `(if condition then)` has a falsey condition
- **THEN** it evaluates to `nil`

#### Scenario: Truthiness
- **WHEN** a value is `false` or `nil`
- **THEN** it is falsey
- **AND** all other values are truthy

#### Scenario: Lexical function closure
- **WHEN** a `fn*` captures locals and is later called
- **THEN** the function body evaluates with captured locals and its closure namespace

#### Scenario: Let bindings
- **WHEN** a `let*` form has name/value binding pairs
- **THEN** bindings are evaluated in order so later binding values can reference earlier bindings
- **AND** the body evaluates with all bindings in scope

#### Scenario: Sequential destructuring let binding
- **WHEN** a `let*` binding pattern is `(list a b)` and the bound value is a two-item list
- **THEN** `a` and `b` are bound to the corresponding list items

#### Scenario: Associative destructuring let binding
- **WHEN** a `let*` binding pattern is `(hash-map "name" n "age" a)` and the bound value is a hash map
- **THEN** `n` and `a` are bound to the values stored under `"name"` and `"age"`

#### Scenario: Missing associative destructuring key
- **WHEN** an associative destructuring key is absent from the bound hash map
- **THEN** the corresponding name is bound to `nil`

#### Scenario: Nested destructuring let binding
- **WHEN** a `let*` binding pattern contains another sequential or associative pattern
- **THEN** nested names are bound by recursively applying the nested pattern to the selected value

#### Scenario: Do body
- **WHEN** a `do` form has multiple body expressions
- **THEN** expressions are evaluated in order and the last expression value is returned

#### Scenario: Destructuring function parameters
- **WHEN** a function parameter is `(list a b)` and the argument is a list
- **THEN** list items are bound to the destructured names
