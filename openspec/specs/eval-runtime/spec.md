# Eval Runtime Spec

## Purpose

Describe the current interpreter target, runtime values, core eval forms, and eval stdlib behavior.

## Requirements

### Requirement: The eval target SHALL return only the final symbol value

The runner SHALL parse and desugar input, evaluate all resulting forms with the eval backend, and return the final result only when that result is a symbol value. It SHALL return an empty output string when there is no final symbol value.

#### Scenario: Evaluate sample test function
- **WHEN** a fixture body is evaluated followed by `(test)`
- **THEN** the printed result is the symbol returned by `test`

#### Scenario: Non-symbol final result
- **WHEN** the final evaluated value is a list, hash map, closure, or no value
- **THEN** the eval runner returns an empty output string

#### Scenario: Eval error result
- **WHEN** evaluation raises `Eval_error` with message `MESSAGE`
- **THEN** the eval runner returns `MESSAGE` as its error string

### Requirement: The evaluator SHALL resolve atoms by the implemented lookup order

Atom lookup SHALL resolve double-quoted string atoms first. For other atoms it SHALL check local bindings, current namespace globals, stdlib bindings, qualified namespace references, and literal names, in that order.

#### Scenario: String atom
- **WHEN** an atom is a double-quoted string such as `"abc"`
- **THEN** it evaluates to symbol value `abc`

#### Scenario: Literal atom
- **WHEN** an atom is `nil`, `true`, `false`, or parses as a floating-point number
- **THEN** it evaluates to a symbol with the atom text after local, global, stdlib, and qualified lookup do not resolve it

#### Scenario: Qualified name after stdlib lookup
- **WHEN** an atom contains `QUALIFIER/MEMBER`
- **THEN** the qualifier is resolved through the current namespace require aliases or used directly as a namespace name
- **AND** the member is read from that namespace's globals

#### Scenario: Missing symbol
- **WHEN** a symbol cannot be resolved
- **THEN** evaluation raises `Eval_error` with `symbol not found: NAME`

### Requirement: The evaluator SHALL support core control and binding forms

The evaluator SHALL support top-level `def`, `compiler/ns`, `deps`, `quote`, `if`, `fn*`, `let*`, and `do` forms. `let*` bindings and `fn*` parameters SHALL accept symbol bindings, sequential destructuring patterns, associative destructuring patterns, and nested combinations of those patterns. Each top-level function parameter pattern SHALL consume one function argument.

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

#### Scenario: Sequential destructuring function parameters
- **WHEN** a function parameter is `(list a b)` and the argument is a list
- **THEN** list items are bound to the destructured names

#### Scenario: Associative and nested function parameters
- **WHEN** a function parameter contains `(hash-map "name" n "tags" (list first-tag))`
- **AND** the argument is a matching hash map
- **THEN** `n` and `first-tag` are bound to their selected values

#### Scenario: Missing function parameter values
- **WHEN** a sequential pattern selects an absent list index or an associative pattern selects an absent hash-map key
- **THEN** the corresponding name is bound to `nil`

#### Scenario: Extra function argument collection values
- **WHEN** a sequential parameter pattern has fewer items than its list argument
- **THEN** unmatched list items are ignored

### Requirement: Evaluator SHALL treat cast as a transparent core form

Evaluator SHALL обработать `(cast TYPE value)` как core-форму, которая игнорирует `TYPE`, вычисляет `value` ровно один раз и возвращает полученное значение без проверки типа.

#### Scenario: Evaluate cast
- **WHEN** evaluated source содержит `(cast java.util.List (str "a" "b"))`
- **THEN** результатом является `ab`
- **AND** evaluator не пытается разрешить символ `java.util.List`

### Requirement: The eval stdlib SHALL provide the implemented functions

The stdlib SHALL expose exactly these eval bindings: `list`, `=`, `vector?`, `concat`, `hash-map`, `get`, `str`, `count`, `map`, `reduce`, `drop`, `+`, `-`, `*`, and `/`.

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

### Requirement: Unknown eval behavior SHALL remain unspecified

This spec SHALL mark behavior as unknown when the current code or tests do not provide a stable language-level answer.

#### Scenario: Nested `def`
- **WHEN** `def` appears outside the supported top-level language convention
- **THEN** the language-level behavior is unknown and this spec does not require callers to rely on it

#### Scenario: Decimal arithmetic
- **WHEN** decimal number symbols are used with eval arithmetic
- **THEN** the language-level numeric model is unknown beyond the current eval stdlib accepting only integer symbol values for arithmetic

#### Scenario: Negative list indexes
- **WHEN** `get` receives a list and a negative integer index
- **THEN** the language-level result and error shape are unknown

#### Scenario: Division by zero
- **WHEN** `/` receives zero as a divisor
- **THEN** the language-level result and error shape are unknown
