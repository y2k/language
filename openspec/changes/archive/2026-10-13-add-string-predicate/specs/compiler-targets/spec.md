# Spec Delta

## ADDED Requirements

### Requirement: Compiler targets SHALL предоставлять предикат string?

JS и Java SHALL предоставлять без пользовательского импорта `(string? value)` для одного значения любого типа. Результат SHALL быть boolean true для строкового runtime-значения, включая пустую строку и keywords, и boolean false для остальных типов. Аргумент SHALL вычисляться ровно один раз без преобразования в текст. Иные арности не входят в переносимый контракт.

#### Scenario: Строки и keywords
- **WHEN** выполняются `(string? "")`, `(string? "text")`, `(string? "nil")`, `(string? "false")`, `(string? "42")`, `(string? :name)` и `(string? (str 42))`
- **THEN** каждый результат — boolean true, как в eval

#### Scenario: Нестроковые значения
- **WHEN** string? получает nil, true, false, целое, дробное число, list/vector, map, функцию, Atom или regex
- **THEN** каждый результат — boolean false без ошибки из-за типа аргумента

#### Scenario: Доступность во вложенном namespace
- **WHEN** программа с namespace `checks.string.core` вызывает `(string? "ok")` без пользовательского импорта предиката
- **THEN** generated JS и Java компилируются с соответствующим runtime и возвращают boolean true

#### Scenario: Проверка полученного значения и однократное вычисление
- **WHEN** string? получает строку из get либо функции, увеличивающей счётчик Atom и возвращающей строку
- **THEN** результат — boolean true
- **AND** при вызове функции счётчик увеличивается ровно один раз

#### Scenario: Boolean вместо текстового результата
- **WHEN** выполняются `(= (string? "") true)`, `(= (string? nil) false)` и `(if (string? 42) "yes" "no")`
- **THEN** результаты — true, true и `"no"`

### Requirement: String? SHALL учитывать строковое представление host и quoted значений

На JS string? SHALL принимать только примитивные строки, а на Java — экземпляры java.lang.String. Цитированные атомы, представленные compiled target как строки, SHALL давать true. Справочник SHALL описывать эти границы и отличие от eval, где цитированные символы, nil, boolean и числа не являются строками.

#### Scenario: Строки из JSON
- **WHEN** JS передаёт string? результаты JSON.parse для JSON-строки, пустой строки, null, числа, boolean, массива и объекта
- **THEN** первые два результата — true, остальные — false

#### Scenario: JS host-значения
- **WHEN** JS передаёт string? объект `new String("x")`, undefined или обычный объект
- **THEN** каждый результат — false

#### Scenario: Java host-значения
- **WHEN** Java передаёт string? экземпляры java.lang.String и java.lang.StringBuilder с одинаковым текстом
- **THEN** результаты — true и false соответственно

#### Scenario: Цитированные атомы compiled targets
- **WHEN** выполняются `(string? 'foo)`, `(string? 'nil)`, `(string? 'false)`, `(string? '42)` и `(string? '"text")`
- **THEN** каждый результат — true на JS и Java
