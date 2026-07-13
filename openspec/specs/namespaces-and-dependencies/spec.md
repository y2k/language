# Namespaces And Dependencies Spec

## Purpose

Describe the current namespace isolation, qualified lookup, and eval dependency loading behavior.

## Requirements

### Requirement: Eval namespaces SHALL isolate top-level definitions

The evaluator SHALL maintain top-level globals per current namespace.

#### Scenario: Current namespace definition
- **WHEN** `(compiler/ns "a" () ())` is followed by `(def x 1)`
- **THEN** `x` is stored in namespace `a`

#### Scenario: Namespace isolation
- **WHEN** `x` is defined in namespace `a` and the current namespace changes to `b`
- **THEN** unqualified `x` is not found in namespace `b`

#### Scenario: Context keeps namespace
- **WHEN** evaluation reuses the same eval context
- **THEN** the current namespace and globals remain available across calls

### Requirement: Qualified lookup SHALL use namespaces and aliases

Qualified symbols SHALL resolve either directly by namespace prefix or through current namespace require aliases.

#### Scenario: Direct qualified lookup
- **WHEN** namespace `a` defines `x`
- **THEN** `a/x` resolves to that value from another namespace

#### Scenario: Alias qualified lookup
- **WHEN** current namespace requires `a` as `aa`
- **THEN** `aa/x` resolves to `a/x`

### Requirement: The deps form SHALL load package source files during eval

The eval backend SHALL load dependency packages from an injected package loader, or from `LY2K_PACKAGES_DIR` when using filesystem loading.

#### Scenario: Load package definitions
- **WHEN** `(deps {:make "0.1.0"})` is evaluated
- **THEN** source files for package `make` version `0.1.0` are parsed, desugared, and evaluated in the current context

#### Scenario: Nested dependencies
- **WHEN** a loaded package file evaluates its own `deps` form
- **THEN** nested package source files are loaded before later dependent definitions are evaluated

#### Scenario: Dependency order
- **WHEN** a symbol is referenced before the `deps` form that defines it
- **THEN** evaluation fails before loading that dependency

#### Scenario: Restore namespace after package load
- **WHEN** a package file changes namespace during loading
- **THEN** the previous current namespace is restored after that file finishes loading

### Requirement: Filesystem package loading SHALL read `.clj` files from package/version directories

The filesystem package loader SHALL read package files from `$LY2K_PACKAGES_DIR/PACKAGE/VERSION` and include only files ending in `.clj`.

#### Scenario: Filesystem dependency path
- **WHEN** package `xml` version `0.3.0` is requested
- **THEN** files are read from `$LY2K_PACKAGES_DIR/xml/0.3.0/*.clj`
