# Compiler Targets Spec

## Purpose

Describe the current runner target dispatch, compiler lowering, JavaScript generation, and Java generation behavior.

## Requirements

### Requirement: The runner SHALL dispatch to eval, JavaScript, and Java targets

The shared runner SHALL parse and desugar input once, then dispatch by target name.

#### Scenario: Known target
- **WHEN** target is `eval`, `js`, or `java`
- **THEN** the runner returns the evaluated output or generated source

#### Scenario: Unknown target
- **WHEN** target is unknown
- **THEN** the runner returns `unknown target: TARGET`

### Requirement: Compiler targets SHALL lower expressions into statement-safe forms

Before JavaScript or Java code generation, compiler targets SHALL lower expression-valued control flow into explicit `do`, `let*`, `if`, and `set!` forms with generated temporaries.

#### Scenario: If expression in value position
- **WHEN** an `if` expression is used where a value is required
- **THEN** lowering creates a generated result binding and assigns branch results before returning that binding

### Requirement: Compiler targets SHALL call local function values

JavaScript и Java compiler targets SHALL разрешать символ в позиции функции как локальное значение, если символ связан параметром или локальным binding. Java target SHALL вызывать такое значение через runtime-интерфейс `FnN`, соответствующий числу аргументов, а локальное binding SHALL иметь приоритет над одноимённой top-level или runtime-функцией.

#### Scenario: Call a lambda passed as a function parameter
- **WHEN** top-level функция принимает лямбду параметром и вызывает этот параметр с поддерживаемой арностью
- **THEN** JavaScript и Java output компилируются и возвращают тот же результат, что и eval

#### Scenario: Call a lambda from a sequential local binding
- **WHEN** функциональное значение связано обычным или созданным destructuring local binding и вызывается в следующей форме lexical body
- **THEN** JavaScript и Java targets вызывают локальное значение
- **AND** Java output компилируется и возвращает тот же результат, что и eval

#### Scenario: Local function value shadows a non-local function
- **WHEN** символ локального функционального значения совпадает с именем top-level или runtime-функции
- **THEN** compiler target вызывает локальное функциональное значение

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

### Requirement: Compiler target runtimes SHALL provide core get

JavaScript и Java runtimes SHALL предоставлять core-функцию `get` для list по числовому индексу и hash-map по ключу. Отсутствующий ключ или индекс SHALL возвращать `nil`/`null`.

#### Scenario: Read list and hash-map values through get
- **WHEN** compiled source вызывает `get` для list по индексу или hash-map по ключу
- **THEN** target runtime возвращает соответствующее значение
- **AND** отсутствующий индекс или ключ возвращает `nil`/`null`

### Requirement: Compiler target runtimes SHALL provide basic two-argument equality

JavaScript и Java runtimes SHALL предоставлять `_EQ_` для сравнения ровно двух значений. Для `nil`, boolean, string и integer значений одного типа результат SHALL быть `true`, когда значения равны, и `false`, когда они различаются.

#### Scenario: Equal scalar values
- **WHEN** compiled source вызывает `(= left right)` с двумя равными `nil`, boolean, string или integer значениями одного типа
- **THEN** JavaScript и Java target возвращают `true`

#### Scenario: Unequal scalar values
- **WHEN** compiled source вызывает `(= left right)` с двумя различными boolean, string или integer значениями одного типа
- **THEN** JavaScript и Java target возвращают `false`

#### Scenario: JavaScript runtime import
- **WHEN** JavaScript source содержит вызов `=`
- **THEN** generated runtime import предоставляет identifier `_EQ_`

### Requirement: Compiler target runtimes SHALL support reduce with an initial value

JavaScript и Java runtimes SHALL поддерживать `(reduce fn init collection)` для list, выполняя left fold по всем элементам `collection` начиная с `init`. Существующая форма `(reduce fn collection)` SHALL сохранить текущее поведение.

#### Scenario: Reduce a non-empty list with an initial value
- **WHEN** compiled source вызывает `(reduce fn init collection)` с непустым list
- **THEN** runtime вызывает `fn` для каждого элемента по порядку как `fn(acc, item)`, начиная с `init`
- **AND** JavaScript и Java возвращают тот же результат, что и evaluator

#### Scenario: Reduce an empty list with an initial value
- **WHEN** compiled source вызывает `(reduce fn init collection)` с пустым list
- **THEN** runtime возвращает `init`
- **AND** runtime не вызывает `fn`

#### Scenario: Preserve two-argument reduce
- **WHEN** compiled source вызывает `(reduce fn collection)` с непустым list
- **THEN** runtime использует первый элемент как initial accumulator и сворачивает оставшиеся элементы
- **AND** вызов с пустым list продолжает завершаться ошибкой

### Requirement: Compiler target runtimes SHALL provide core drop

JavaScript и Java runtimes SHALL предоставлять core-функцию `drop`, которая принимает целое число и list и возвращает list без первых указанного количества элементов.

#### Scenario: Drop items from a list
- **WHEN** compiled source вызывает `(drop n items)` с положительным целым `n`, меньшим длины `items`
- **THEN** target runtime возвращает оставшиеся элементы в исходном порядке
- **AND** JavaScript и Java возвращают тот же результат, что и evaluator

#### Scenario: Drop with boundary counts
- **WHEN** `n` меньше или равно нулю
- **THEN** target runtime возвращает все элементы list
- **WHEN** `n` не меньше длины list
- **THEN** target runtime возвращает пустой list

### Requirement: The JavaScript compiler SHALL emit ES module code using the language runtime

The JavaScript target SHALL emit an import from `./language_runtime.js` and compile language forms to JavaScript expressions/statements. Символьный namespace require SHALL компилироваться в относительный module specifier с расширением `.js`; строковый `:require` SHALL сохранять свой module specifier как bare ESM import без добавления `./` или `.js`. Форма `(export-default expression)` SHALL компилироваться в статическую ESM-декларацию `export default <expression>` вместо вызова `export_default(...)`.

#### Scenario: Runtime import
- **WHEN** JavaScript source is generated
- **THEN** it begins with imports for runtime functions such as `list`, `hash_map`, `truthy`, `str`, arithmetic, `count`, `map`, and `reduce`

#### Scenario: Namespace requires
- **WHEN** source contains `(:require [io.math.core :as mc])`
- **THEN** JavaScript emits `import * as mc from "./io/math/core.js"`

#### Scenario: String Node module require
- **WHEN** source contains `(:require ["node:test" :as t])`
- **THEN** JavaScript emits `import * as t from "node:test"`

#### Scenario: String npm package require
- **WHEN** source contains `(:require ["wrangler" :as w])`
- **THEN** JavaScript emits `import * as w from "wrangler"`

#### Scenario: Definitions and functions
- **WHEN** a top-level `def` contains a function
- **THEN** JavaScript emits a `const` binding to an arrow function

#### Scenario: JavaScript interop syntax
- **WHEN** source uses `new` or `.` forms
- **THEN** JavaScript emits constructor calls and instance method calls

#### Scenario: Default export
- **WHEN** source contains `(export-default {:fetch handler})`
- **THEN** JavaScript emits `export default (hash_map)("fetch", handler);`
- **AND** output does not invoke `export_default`

### Requirement: JavaScript compiler SHALL compile cast as a transparent expression

JavaScript compiler SHALL compile `(cast TYPE value)` as compiled `value`, не выполняя проверку `TYPE` и не генерируя runtime-зависимость для cast. `value` SHALL вычисляться ровно один раз согласно обычной семантике выражений JavaScript.

#### Scenario: Compile JavaScript cast
- **WHEN** source содержит `(cast java.util.List xs)`
- **THEN** JavaScript output содержит скомпилированное выражение `xs` без вызова или идентификатора `cast`

### Requirement: The Java compiler SHALL emit a Java helper class using the language runtime

The Java target SHALL emit Java source with a static import of `y2k.language.language_runtime.*` and a generated helper class.

#### Scenario: No namespace
- **WHEN** no namespace is declared
- **THEN** Java emits class `user`

#### Scenario: Namespace package and class
- **WHEN** namespace is `app.main`
- **THEN** Java emits package `app` and class `main`

#### Scenario: Top-level functions
- **WHEN** top-level `def` defines a `fn*`
- **THEN** Java emits a static method whose parameters are `Object`

#### Scenario: Qualified requires
- **WHEN** a required alias is used as `alias/member`
- **THEN** Java emits a call to the required namespace class and member

#### Scenario: Java imports
- **WHEN** namespace imports include Java classes
- **THEN** Java emits corresponding `import` statements

### Requirement: The Java compiler SHALL support Java interop forms

The Java target SHALL compile casts, constructors, method calls, annotated lambdas, and `gen-class` forms.

#### Scenario: Cast form
- **WHEN** source contains `(cast java.util.List xs)`
- **THEN** Java emits a Java cast expression

#### Scenario: Typed lambda annotation
- **WHEN** a `fn*` has metadata such as `^java.util.function.Function`
- **THEN** Java emits a lambda cast to the annotated target type

#### Scenario: Void typed lambda annotation
- **WHEN** a `fn*` has metadata `^void:java.lang.Runnable`
- **THEN** Java emits a void lambda body

#### Scenario: Invalid void annotation
- **WHEN** metadata is `^void:` with no target type
- **THEN** Java compilation fails with `Java lambda annotation ^void: requires a target type`

#### Scenario: gen-class
- **WHEN** source contains a valid `gen-class` declaration
- **THEN** Java emits a nested public static class extending the requested base class and forwarding declared methods to helper functions

### Requirement: Java compiler SHALL honor type hints on let bindings

Java target SHALL использовать `^TYPE` metadata символьного `let` binding для приведения RHS к `TYPE` перед созданием локальной переменной. RHS SHALL вычисляться ровно один раз в обычном последовательном порядке `let`, а последующие Java interop expressions SHALL видеть локальную переменную с указанным Java-типом. Это требование не применяется к destructuring patterns и не меняет поведение eval или JavaScript targets.

#### Scenario: Вызов Java-метода через hinted binding

- **WHEN** source содержит `(let [^java.lang.String text (make-text)] (.concat text "!"))`, где `make-text` компилируется с Java return type `Object`
- **THEN** Java output приводит результат `make-text` к `java.lang.String` перед созданием `text`
- **AND** generated Java компилируется и вызов `concat` возвращает ожидаемую строку
