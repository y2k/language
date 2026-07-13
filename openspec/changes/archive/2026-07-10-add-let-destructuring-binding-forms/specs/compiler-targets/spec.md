## ADDED Requirements

### Requirement: Lowering SHALL eliminate destructuring let bindings

Lowering SHALL преобразовывать `let*` bindings с sequential или associative patterns в bindings только с symbol слева. Для каждого destructuring RHS lowering SHALL вычислять RHS ровно один раз во временное имя через `Gensym`, а затем SHALL создавать leaf bindings с core-вызовами `get`. JavaScript и Java generators SHALL не содержать специальной логики для destructuring patterns.

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

### Requirement: Compiler target runtimes SHALL provide core get

JavaScript и Java runtimes SHALL предоставлять core-функцию `get` для list по числовому индексу и hash-map по ключу. Отсутствующий ключ или индекс SHALL возвращать `nil`/`null`.

#### Scenario: Read list and hash-map values through get
- **WHEN** compiled source вызывает `get` для list по индексу или hash-map по ключу
- **THEN** target runtime возвращает соответствующее значение
- **AND** отсутствующий индекс или ключ возвращает `nil`/`null`
