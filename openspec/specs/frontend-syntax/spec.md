# Frontend Syntax Spec

## Purpose

Describe the current parser, AST metadata, macro expansion, and namespace macro behavior.

## Requirements

### Requirement: The parser SHALL produce s-expressions with source metadata

The parser SHALL read source text into atoms and lists while preserving line/column metadata on parsed nodes.

#### Scenario: Parse multiple top-level forms
- **WHEN** the input contains multiple valid forms separated by whitespace
- **THEN** parsing returns all top-level forms in source order

#### Scenario: Track locations
- **WHEN** a parsed atom or list begins at a source offset
- **THEN** its metadata records the corresponding 1-based line and column

### Requirement: The parser SHALL ignore whitespace and line comments

Whitespace and `;` line comments SHALL be skipped between forms.

#### Scenario: Ignore semicolon comments
- **WHEN** a line contains a `;` comment
- **THEN** characters through the next newline are ignored by the parser

### Requirement: The parser SHALL support atom, string, quote, and bracket forms

The parser SHALL support atoms, double-quoted string atoms, quote shorthand, and lists delimited by `()`, `[]`, and `{}`.

#### Scenario: Quote shorthand
- **WHEN** the input contains `'x`
- **THEN** the parser produces `(quote x)`

#### Scenario: Bracket-specific lists
- **WHEN** the input contains `(a)`, `[a]`, or `{a}`
- **THEN** the AST records whether the list used parens, brackets, or braces

### Requirement: The parser SHALL attach type annotation metadata

The parser SHALL attach `^TYPE` metadata to the immediately following atom or list.

#### Scenario: Annotate a function expression
- **WHEN** the input contains `^java.util.function.Function (fn [x] x)`
- **THEN** the function list metadata contains the annotation string

### Requirement: Built-in macros SHALL desugar user syntax into core forms

Macro expansion SHALL transform recognized syntactic forms before backend execution or compilation. Binding forms in `let` SHALL preserve pattern structure after normal collection and keyword expansion so backends can distinguish symbol, sequential, and associative binding patterns.

#### Scenario: Desugar collection literals
- **WHEN** the input contains `[1 2]` or `{:a 1}`
- **THEN** vectors become `(list 1 2)` and maps become `(hash-map "a" 1)` after keyword expansion

#### Scenario: Desugar binding and function forms
- **WHEN** the input contains `let`, `fn`, `defn`, or `defn-`
- **THEN** they become `let*`, `fn*`, or `def` plus `fn`

#### Scenario: Preserve sequential let binding patterns
- **WHEN** the input contains `(let [[a b] xs] body)`
- **THEN** macro expansion preserves the binding pattern as a sequential pattern in the resulting `let*` bindings

#### Scenario: Preserve associative let binding patterns
- **WHEN** the input contains `(let [{:name n :age a} user] body)`
- **THEN** macro expansion preserves the binding pattern as an associative pattern in the resulting `let*` bindings

#### Scenario: Preserve short-circuit behavior in logical macros
- **WHEN** the input contains `and` or `or`
- **THEN** expansion uses generated temporaries, `let*`, and `if` so operands are evaluated at most once and only as needed

#### Scenario: Desugar threading macros
- **WHEN** the input contains `->` or `->>`
- **THEN** forms are rewritten by inserting the threaded value as first or last argument respectively

#### Scenario: Desugar interop shorthand
- **WHEN** the input contains `(.method obj args...)` or `(Class. args...)`
- **THEN** it becomes `(. obj method args...)` or `(new Class args...)`

### Requirement: Макросы SHALL дешугорировать аннотации типов символьных параметров

При дешугорировании `fn` и производных от него `defn` или `defn-` аннотация `^TYPE` на символьном параметре SHALL создавать fresh формальный параметр и локальный `let*` binding исходного имени к `(cast TYPE FRESH)`. Макрос SHALL сохранять порядок параметров, арность функции и тело после initial bindings. Это требование применяется только к символьным параметрам.

#### Scenario: Аннотированный параметр fn
- **WHEN** вход содержит `(fn [^java.util.List xs] (.size xs))`
- **THEN** результат содержит `fn*` с одним fresh символьным параметром и local binding `xs` к `(cast java.util.List FRESH)` до вызова `.size`

#### Scenario: Несколько аннотированных параметров
- **WHEN** вход содержит функцию с двумя символьными параметрами, аннотированными разными типами
- **THEN** результат сохраняет два формальных параметра и создаёт local cast binding для каждого исходного имени в исходном порядке

#### Scenario: Аннотированный параметр defn
- **WHEN** вход содержит `(defn size [^java.util.List xs] (.size xs))`
- **THEN** функция, созданная после дешугорирования `defn`, содержит local cast binding для `xs`

### Requirement: The namespace macro SHALL lower namespace declarations

The `ns` macro SHALL lower namespace declarations into `compiler/ns` with string namespace, require pairs, and import pairs. An `:import` clause SHALL accept one or more bracket import vectors; each vector SHALL contain a package symbol followed by zero or more class symbols. The macro SHALL emit import pairs in source order across vectors and classes.

#### Scenario: Lower require aliases
- **WHEN** the input contains `(ns app.main (:require [io.math.core :as mc]))`
- **THEN** it becomes a `compiler/ns` form with the namespace `io.math.core` paired to alias `mc`

#### Scenario: Lower import classes
- **WHEN** the input contains `(ns app.main (:import [java.time LocalDate]))`
- **THEN** it records `LocalDate` mapped to `java.time.LocalDate`

#### Scenario: Lower multiple import vectors
- **WHEN** the input contains `(ns app.main (:import [java.time LocalDate] [java.util UUID]))`
- **THEN** it records `LocalDate` mapped to `java.time.LocalDate` followed by `UUID` mapped to `java.util.UUID`
