## MODIFIED Requirements

### Requirement: Lowering SHALL eliminate destructuring let bindings

Lowering SHALL преобразовывать `let*` bindings и `fn*` parameter patterns с sequential или associative patterns в bindings только с symbol слева. Для каждого destructuring `let*` RHS lowering SHALL вычислять RHS ровно один раз во временное имя через `Gensym`, а для каждого non-symbol parameter pattern SHALL создавать один fresh symbol parameter через `Gensym` и initial bindings в function body. Затем lowering SHALL создавать leaf bindings с core-вызовами `get`. JavaScript и Java generators SHALL не содержать специальной логики для destructuring patterns.

#### Scenario: Lower sequential destructuring let binding
- **WHEN** source содержит `(let [[a b] (list "x" "y")] (str a b))`
- **THEN** lowering передаёт generator только symbol bindings для временного значения, `a` и `b`

#### Scenario: Lower associative destructuring let binding
- **WHEN** source содержит `(let [{:name n :age a} (hash-map "name" "Ada" "age" "36")] (str n a))`
- **THEN** lowering передаёт generator только symbol bindings, использующие `get` с ключами `"name"` и `"age"`

#### Scenario: Lower nested destructuring let binding
- **WHEN** source содержит `let` binding с вложенными sequential или associative patterns
- **THEN** lowering рекурсивно создаёт symbol bindings для всех leaf symbols

#### Scenario: Preserve single evaluation of destructured value
- **WHEN** destructuring `let*` RHS имеет побочный эффект или вызывает функцию
- **THEN** JavaScript и Java выполняют RHS ровно один раз до выполнения leaf bindings

#### Scenario: Lower sequential function parameter pattern
- **WHEN** source содержит `(fn [[a b]] (str a b))`
- **THEN** lowering передаёт generator один fresh symbol parameter и symbol `let*` bindings для `a` и `b`
- **AND** generated function сохраняет arity в один аргумент

#### Scenario: Lower associative and nested function parameter pattern
- **WHEN** source содержит `(fn [{:name n :tags [tag]}] (str n tag))`
- **THEN** lowering передаёт generator только symbol parameters and bindings
- **AND** leaf bindings используют `get` для каждого map key и list index

#### Scenario: Compile destructuring function parameters on all targets
- **WHEN** a `fn` or `defn` uses sequential, associative, or nested parameter patterns
- **THEN** JavaScript and Java compile the function without generator-specific destructuring support
- **AND** compiled execution produces the same bindings as eval
