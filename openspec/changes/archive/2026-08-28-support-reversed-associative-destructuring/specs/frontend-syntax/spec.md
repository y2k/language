## MODIFIED Requirements

### Requirement: Built-in macros SHALL desugar user syntax into core forms

Macro expansion SHALL transform recognized syntactic forms before backend execution or compilation. После каждого раскрытия outer form SHALL снова проверяться built-in macros до тех пор, пока она не перестанет быть распознаваемой macro form; metadata результирующей формы SHALL сохраняться в последующих раскрытиях. Binding forms in `let` SHALL preserve pattern structure after normal collection and keyword expansion so backends can distinguish symbol, sequential, and associative binding patterns. Brace binding patterns in `let` and `fn` SHALL recursively normalize each reverse pair `binding :keyword` to the existing key-first form `:keyword binding` before collection and keyword expansion. Эта нормализация SHALL не применяться к обычным map-выражениям, прямым core-формам `let*`/`fn*` или явно записанным `(hash-map ...)` patterns. `(def- name value)` SHALL become core `(def name value)` with private metadata. `(defn- name params body...)` SHALL become private `(defn name params body...)` and complete the normal `defn` and `fn` expansions into a private core `def` containing `fn*`. Двухаргументная форма `(:key collection)` SHALL раскрывать keyword-вызов в `(get collection "key")` до обычного преобразования keyword в строковый литерал.

#### Scenario: Desugar collection literals
- **WHEN** the input contains `[1 2]` or `{:a 1}`
- **THEN** vectors become `(list 1 2)` and maps become `(hash-map "a" 1)` after keyword expansion

#### Scenario: Desugar keyword lookup
- **WHEN** the input contains `(:TELEGRAM_WEBHOOK_SECRET env)`
- **THEN** it becomes `(get env "TELEGRAM_WEBHOOK_SECRET")`

#### Scenario: Desugar binding and function forms
- **WHEN** the input contains `let`, `fn`, or `defn`
- **THEN** they become `let*`, `fn*`, or a non-private `def` plus `fn*`

#### Scenario: Desugar private value definition
- **WHEN** the input contains `(def- storage value)`
- **THEN** it becomes a core `def` of `storage` with `value` and private metadata

#### Scenario: Desugar private function definition through public function macro
- **WHEN** the input contains `(defn- helper [x] x)`
- **THEN** it first becomes a private `defn`, then a private core `def` containing `fn*`

#### Scenario: Preserve private metadata through chained macro expansion
- **WHEN** a macro expansion produces another recognized macro form with private metadata
- **THEN** the later expansion receives and preserves that private metadata

#### Scenario: Preserve sequential let binding patterns
- **WHEN** the input contains `(let [[a b] xs] body)`
- **THEN** macro expansion preserves the binding pattern as a sequential pattern in the resulting `let*` bindings

#### Scenario: Preserve associative let binding patterns
- **WHEN** the input contains `(let [{:name n :age a} user] body)`
- **THEN** macro expansion preserves the binding pattern as an associative pattern in the resulting `let*` bindings

#### Scenario: Normalize reversed associative let bindings
- **WHEN** the input contains `(let [{url :url props :props} value] body)`
- **THEN** macro expansion produces canonical bindings `(hash-map "url" url "props" props)` in the resulting `let*`

#### Scenario: Normalize reversed associative function parameters
- **WHEN** a `fn`, `defn`, or `defn-` parameter contains `{url :url}`
- **THEN** macro expansion produces a canonical `(hash-map "url" url)` parameter pattern in the resulting `fn*`

#### Scenario: Normalize nested reversed associative bindings
- **WHEN** a `let` binding or `fn` parameter contains reverse keyword pairs nested in sequential or associative brace patterns
- **THEN** every nested reverse keyword pair is normalized to key-first order

#### Scenario: Leave map expressions outside binding patterns unchanged
- **WHEN** `{url :url}` occurs as a value expression rather than a `let` binding pattern or `fn` parameter
- **THEN** macro expansion preserves its pair order and produces `(hash-map url "url")`

#### Scenario: Preserve short-circuit behavior in logical macros
- **WHEN** the input contains `and` or `or`
- **THEN** expansion uses generated temporaries, `let*`, and `if` so operands are evaluated at most once and only as needed

#### Scenario: Desugar threading macros
- **WHEN** the input contains `->` or `->>`
- **THEN** forms are rewritten by inserting the threaded value as first or last argument respectively

#### Scenario: Desugar interop shorthand
- **WHEN** the input contains `(.method obj args...)` or `(Class. args...)`
- **THEN** it becomes `(. obj method args...)` or `(new Class args...)`
