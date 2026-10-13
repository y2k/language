# Spec Delta

## ADDED Requirements

### Requirement: Eval SHALL предоставлять предикат string?

Eval SHALL предоставлять без импорта `(string? value)` для одного значения любого типа. Результат SHALL быть boolean true только для строкового runtime-значения, включая пустую строку и keywords, и boolean false для остальных типов. Преобразование аргумента в текст SHALL NOT выполняться. Аргумент SHALL вычисляться ровно один раз.

#### Scenario: Строки и keywords
- **WHEN** вычисляются `(string? "")`, `(string? "text")`, `(string? "nil")`, `(string? "false")`, `(string? "42")`, `(string? :name)` и `(string? (str 42))`
- **THEN** каждый результат — boolean true

#### Scenario: Нестроковые значения
- **WHEN** string? получает nil, true, false, целое, дробное число, list/vector, map, функцию, Atom или regex
- **THEN** каждый результат — boolean false
- **AND** проверка не вызывает ошибку из-за типа аргумента

#### Scenario: Цитированные значения eval
- **WHEN** вычисляются `(string? 'foo)`, `(string? 'nil)`, `(string? 'false)`, `(string? '42)` и `(string? '"text")`
- **THEN** результаты — false, false, false, false и true соответственно

#### Scenario: Проверка полученного значения и однократное вычисление
- **WHEN** string? получает строку из get либо функции, увеличивающей счётчик Atom и возвращающей строку
- **THEN** результат — boolean true
- **AND** при вызове функции счётчик увеличивается ровно один раз

#### Scenario: Boolean вместо текстового результата
- **WHEN** вычисляются `(= (string? "") true)`, `(= (string? nil) false)` и `(if (string? 42) "yes" "no")`
- **THEN** результаты — true, true и `"no"`

### Requirement: Eval SHALL проверять арность string?

Вызов string? с числом аргументов, отличным от одного, SHALL завершаться Eval_error с именем string? и указанием ожидаемого одного аргумента. Точный текст сообщения не фиксируется.

#### Scenario: Отсутствующий и лишний аргумент
- **WHEN** выполняется `(string?)` либо `(string? "a" "b")`
- **THEN** возникает Eval_error, называющий string? и ожидаемую арность
