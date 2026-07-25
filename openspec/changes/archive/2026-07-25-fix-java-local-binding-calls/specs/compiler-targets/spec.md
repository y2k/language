## MODIFIED Requirements

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
