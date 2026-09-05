## MODIFIED Requirements

### Requirement: The eval stdlib SHALL provide the implemented functions

Stdlib SHALL экспортировать ровно следующие eval bindings: `list`, `=`, `not`, `>`, `<`, `>=`, `<=`, `vector?`, `concat`, `hash-map`, `get`, `str`, `slurp`, `count`, `map`, `reduce`, `drop`, `+`, `-`, `*` и `/`. `slurp` SHALL разрешать relative path от текущего рабочего каталога процесса.

#### Scenario: Lists and hash maps
- **WHEN** `list` вызывается с любыми аргументами
- **THEN** функция возвращает runtime list с этими аргументами в исходном порядке
- **WHEN** `hash-map` вызывается с чётным числом аргументов
- **THEN** функция возвращает runtime hash map с парами key/value в порядке аргументов
- **WHEN** `hash-map` вызывается с нечётным числом аргументов
- **THEN** evaluation вызывает `Eval_error` с `hash-map arguments must be key/value pairs`

#### Scenario: Equality
- **WHEN** `=` получает ноль или одно значение
- **THEN** функция возвращает `true`
- **WHEN** `=` получает несколько значений
- **THEN** функция возвращает `true` только тогда, когда все runtime values структурно равны

#### Scenario: Logical negation
- **WHEN** `not` получает `false` или `nil`
- **THEN** функция возвращает `true`
- **WHEN** `not` получает любое другое runtime value
- **THEN** функция возвращает `false`

#### Scenario: Integer comparisons
- **WHEN** `>` или `<` получает два целочисленных symbol values, представимых evaluator
- **THEN** функция возвращает `true` ровно тогда, когда первое значение соответственно больше или меньше второго
- **WHEN** `>=` или `<=` получает два целочисленных symbol values, представимых evaluator
- **THEN** функция возвращает `true` ровно тогда, когда первое значение соответственно больше либо равно или меньше либо равно второму

#### Scenario: Count collections
- **WHEN** `count` получает один list или hash map
- **THEN** функция возвращает число элементов list или пар hash map как symbol value

#### Scenario: Concatenate lists
- **WHEN** `concat` получает только lists
- **THEN** функция возвращает один runtime list со всеми элементами в исходном порядке

#### Scenario: Vector predicate
- **WHEN** `vector?` получает runtime list
- **THEN** функция возвращает `true`
- **WHEN** `vector?` получает одно значение, не являющееся list
- **THEN** функция возвращает `false`

#### Scenario: Get from collections
- **WHEN** `get` вызывается с hash map и key
- **THEN** функция возвращает связанное значение или `nil`
- **WHEN** `get` вызывается с list и неотрицательным целочисленным index
- **THEN** функция возвращает элемент по этому index или `nil`

#### Scenario: Map and reduce
- **WHEN** `map` получает function и list
- **THEN** функция возвращает list с результатом function для каждого элемента
- **WHEN** `reduce` получает function и непустую collection без init
- **THEN** функция выполняет fold, начиная с первого элемента
- **WHEN** `reduce` получает function, init value и collection
- **THEN** функция выполняет fold всех элементов, начиная с init

#### Scenario: Drop items
- **WHEN** `drop` получает целое число и list
- **THEN** функция возвращает list без указанного числа первых элементов
- **AND** число меньше или равное нулю возвращает исходный list

#### Scenario: String conversion
- **WHEN** `str` получает runtime values
- **THEN** функция возвращает одно symbol value, объединяя symbols как их текст, lists как строки элементов в круглых скобках, hash maps как строки key/value в фигурных скобках в порядке хранения, а closures как `#<function>`

#### Scenario: Чтение текстового файла
- **WHEN** `(slurp "notes.txt")` исполняется из рабочего каталога, где `notes.txt` доступен для чтения и содержит несколько строк
- **THEN** функция разрешает `notes.txt` относительно этого рабочего каталога
- **AND** возвращает symbol value с полным содержимым файла, включая переводы строк

#### Scenario: Неверные аргументы slurp
- **WHEN** `slurp` получает не ровно один symbol value
- **THEN** evaluation вызывает `Eval_error` с `slurp expects one path`

#### Scenario: Ошибка чтения файла
- **WHEN** `slurp` не может открыть или прочитать файл по `PATH`
- **THEN** evaluation вызывает `Eval_error` с `slurp failed: PATH`

#### Scenario: Arithmetic
- **WHEN** arithmetic stdlib functions получают целочисленные symbol values
- **THEN** они возвращают целочисленные symbol values
- **AND** `/` использует целочисленное деление
