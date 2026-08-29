## MODIFIED Requirements

### Requirement: The parser SHALL support atom, string, quote, and bracket forms

The parser SHALL support atoms, double-quoted string atoms, quote shorthand, and lists delimited by `()`, `[]`, and `{}`. Within a double-quoted source string, the two source characters `\n` SHALL decode to one LF character in the parsed string value before evaluation or target compilation.

#### Scenario: Quote shorthand
- **WHEN** the input contains `'x`
- **THEN** the parser produces `(quote x)`

#### Scenario: Bracket-specific lists
- **WHEN** the input contains `(a)`, `[a]`, or `{a}`
- **THEN** the AST records whether the list used parens, brackets, or braces

#### Scenario: Decode newline escape
- **WHEN** the input contains the quoted source string `"a\nb"`, where `\n` consists of backslash followed by `n`
- **THEN** the parsed string value contains exactly one LF between `a` and `b`
- **AND** it does not contain a literal backslash followed by `n`

#### Scenario: Preserve newline value across targets
- **WHEN** a parsed string containing the decoded LF is evaluated or compiled for JavaScript or Java
- **THEN** every target observes a runtime string containing one LF at that position
