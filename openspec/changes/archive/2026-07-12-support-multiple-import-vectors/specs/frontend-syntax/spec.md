## MODIFIED Requirements

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
