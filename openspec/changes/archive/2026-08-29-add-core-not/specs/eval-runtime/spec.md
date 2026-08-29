## MODIFIED Requirements

### Requirement: The eval stdlib SHALL provide the implemented functions

The stdlib SHALL expose exactly these eval bindings: `list`, `=`, `not`, `>`, `<`, `>=`, `<=`, `vector?`, `concat`, `hash-map`, `get`, `str`, `count`, `map`, `reduce`, `drop`, `+`, `-`, `*`, and `/`.

#### Scenario: Lists and hash maps
- **WHEN** `list` is called with any arguments
- **THEN** it returns a runtime list containing those arguments in order
- **WHEN** `hash-map` is called with an even number of arguments
- **THEN** it returns a runtime hash map containing key/value pairs in argument order
- **WHEN** `hash-map` is called with an odd number of arguments
- **THEN** it raises `Eval_error` with `hash-map arguments must be key/value pairs`

#### Scenario: Equality
- **WHEN** `=` receives zero or one value
- **THEN** it returns `true`
- **WHEN** `=` receives multiple values
- **THEN** it returns `true` only when all values are structurally equal runtime values

#### Scenario: Logical negation
- **WHEN** `not` receives `false` or `nil`
- **THEN** it returns `true`
- **WHEN** `not` receives any other runtime value
- **THEN** it returns `false`

#### Scenario: Integer comparisons
- **WHEN** `>` or `<` receives two integer symbol values representable by evaluator
- **THEN** it returns `true` exactly when the first value is respectively greater than or less than the second value
- **WHEN** `>=` or `<=` receives two integer symbol values representable by evaluator
- **THEN** it returns `true` exactly when the first value is respectively greater than or equal to or less than or equal to the second value

#### Scenario: Count collections
- **WHEN** `count` receives one list or hash map
- **THEN** it returns the number of list items or hash map pairs as a symbol value

#### Scenario: Concatenate lists
- **WHEN** `concat` receives only lists
- **THEN** it returns one runtime list containing all items in order

#### Scenario: Vector predicate
- **WHEN** `vector?` receives a runtime list
- **THEN** it returns `true`
- **WHEN** `vector?` receives one non-list value
- **THEN** it returns `false`

#### Scenario: Get from collections
- **WHEN** `get` is called with a hash map and key
- **THEN** it returns the associated value or `nil`
- **WHEN** `get` is called with a list and non-negative integer index
- **THEN** it returns the item at that index or `nil`

#### Scenario: Map and reduce
- **WHEN** `map` receives a function and list
- **THEN** it returns a list containing the function result for each item
- **WHEN** `reduce` receives a function and non-empty collection without init
- **THEN** it folds from the first item
- **WHEN** `reduce` receives a function, init value, and collection
- **THEN** it folds every collection item starting from the init value

#### Scenario: Drop items
- **WHEN** `drop` receives an integer count and a list
- **THEN** it returns the list without the first count items
- **AND** counts less than or equal to zero return the original list

#### Scenario: String conversion
- **WHEN** `str` receives runtime values
- **THEN** it returns one symbol value by concatenating symbols as their text, lists as parenthesized item strings, hash maps as braced key/value strings in stored order, and closures as `#<function>`

#### Scenario: Arithmetic
- **WHEN** arithmetic stdlib functions receive integer symbol values
- **THEN** they return integer symbol values
- **AND** `/` uses integer division
