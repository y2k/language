## MODIFIED Requirements

### Requirement: The eval stdlib SHALL provide the implemented functions

Stdlib SHALL предоставлять ровно следующие eval bindings: `list`, `=`, `not=`, `not`, `assert`, `>`, `<`, `>=`, `<=`, `vector?`, `concat`, `hash-map`, `get`, `str`, `slurp`, `count`, `map`, `reduce`, `drop`, `+`, `-`, `*`, `/`, `atom`, `deref`, `reset!` и `swap!`. `slurp` SHALL разрешать relative path от текущего рабочего каталога процесса. Четыре функции Atom SHALL соответствовать контракту capability `atoms`.

#### Scenario: Lists and hash maps
- **WHEN** `list` вызывается с любыми аргументами
- **THEN** возвращается runtime list с этими аргументами в исходном порядке
- **WHEN** `hash-map` вызывается с чётным числом аргументов
- **THEN** возвращается runtime hash-map с парами ключ/значение в исходном порядке
- **WHEN** `hash-map` вызывается с нечётным числом аргументов
- **THEN** возникает `Eval_error` с `hash-map arguments must be key/value pairs`

#### Scenario: Equality
- **WHEN** `=` получает ноль или одно значение
- **THEN** возвращается `true`
- **WHEN** `=` получает несколько значений
- **THEN** возвращается `true` только если все значения структурно равны согласно правилам runtime

#### Scenario: Two-argument scalar inequality
- **WHEN** `not=` получает ровно два поддерживаемых `=` значения типа `nil`, boolean, string или integer
- **THEN** возвращается `false`, если `=` возвращает `true`
- **AND** возвращается `true`, если `=` возвращает `false`

#### Scenario: Logical negation
- **WHEN** `not` получает `false` или `nil`
- **THEN** возвращается `true`
- **WHEN** `not` получает любое другое runtime value
- **THEN** возвращается `false`

#### Scenario: Integer comparisons
- **WHEN** `>` или `<` получает два целочисленных symbol values, представимых evaluator
- **THEN** возвращается `true` ровно когда первое число соответственно больше или меньше второго
- **WHEN** `>=` или `<=` получает два таких числа
- **THEN** возвращается `true` ровно когда первое число соответственно больше либо равно или меньше либо равно второму

#### Scenario: Count collections
- **WHEN** `count` получает один list или hash-map
- **THEN** возвращается число элементов list или пар hash-map как symbol value

#### Scenario: Concatenate lists
- **WHEN** `concat` получает только lists
- **THEN** возвращается один list со всеми элементами в исходном порядке

#### Scenario: Vector predicate
- **WHEN** `vector?` получает runtime list
- **THEN** возвращается `true`
- **WHEN** `vector?` получает одно значение другого типа
- **THEN** возвращается `false`

#### Scenario: Get from collections
- **WHEN** `get` вызывается с hash-map и ключом
- **THEN** возвращается связанное значение либо `nil`
- **WHEN** `get` вызывается с list и неотрицательным целочисленным индексом
- **THEN** возвращается элемент по этому индексу либо `nil`

#### Scenario: Map and reduce
- **WHEN** `map` получает функцию и list
- **THEN** возвращается list результатов вызова функции для каждого элемента
- **WHEN** `reduce` получает функцию и непустую коллекцию без init
- **THEN** свёртка начинается с первого элемента
- **WHEN** `reduce` получает функцию, init и коллекцию
- **THEN** все элементы сворачиваются начиная с init

#### Scenario: Drop items
- **WHEN** `drop` получает целочисленный count и list
- **THEN** возвращается list без первых count элементов
- **AND** count меньше либо равный нулю возвращает исходный list

#### Scenario: String conversion
- **WHEN** `str` получает runtime values
- **THEN** возвращается один symbol value, объединяющий текст symbols, lists со строками элементов в круглых скобках, hash-maps со строками ключей и значений в фигурных скобках в сохранённом порядке и closures как `#<function>`

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
- **WHEN** арифметические stdlib functions получают целочисленные symbol values
- **THEN** возвращаются целочисленные symbol values
- **AND** `/` использует целочисленное деление

#### Scenario: Функции Atom доступны без импорта
- **WHEN** программа вызывает `atom`, `deref`, `reset!` или `swap!` без пользовательских определений этих имён
- **THEN** evaluator разрешает их через stdlib и выполняет согласно capability `atoms`
