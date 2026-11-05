## MODIFIED Requirements

### Requirement: The eval stdlib SHALL provide the implemented functions

Stdlib SHALL предоставлять bindings `list`, `=`, `not=`, `not`, `assert`, `>`, `<`, `>=`, `<=`, `vector?`, `concat`, `hash-map`, `get`, `get-in`, `str`, `slurp`, `count`, `map`, `reduce`, `drop`, `+`, `-`, `*`, `/`, `atom`, `deref`, `reset!`, `swap!`. Этот перечень SHALL NOT исключать bindings других требований. Числовые операции SHALL принимать числовые значения, не текст; путь slurp SHALL быть строкой, не числом/символом. `slurp` SHALL разрешать relative path от cwd процесса. Типы SHALL сохраняться в коллекциях, функциях и Atom.

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
- **THEN** возвращает true только если все равны: числа сравниваются по значению, остальные скаляры — по типу и содержимому, lists/maps — рекурсивно с учётом порядка
- **AND** функции и Atom считаются неравными, включая сравнение с собой

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
- **WHEN** `>` или `<` получает два целых числа
- **THEN** возвращается `true` ровно когда первое число соответственно больше или меньше второго
- **WHEN** `>=` или `<=` получает два таких числа
- **THEN** возвращается `true` ровно когда первое число соответственно больше либо равно или меньше либо равно второму
- **AND** строки и binary64-аргументы, даже `1.0`, отклоняются с Eval_error

#### Scenario: Count collections
- **WHEN** `count` получает один list или hash-map
- **THEN** возвращается целое число элементов list или пар hash-map, не строка

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
- **THEN** возвращается значение первой пары с ключом, равным по правилам =, либо nil
- **WHEN** `get` вызывается с list и неотрицательным целочисленным индексом
- **THEN** возвращается элемент по этому индексу либо `nil`
- **AND** строковый, символьный или binary64-индекс отклоняется с Eval_error

#### Scenario: Get from nil
- **WHEN** вычисляется `(get nil :text)` либо `(:text nil)`
- **THEN** результат — nil

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
- **AND** строковые и binary64-count отклоняются с Eval_error

#### Scenario: String conversion
- **WHEN** `str` получает runtime values
- **THEN** возвращает строку, объединяя текст строк и символов без кавычек, чисел, boolean и nil; lists оформляются круглыми скобками, maps — фигурными с сохранением порядка пар, функции — `#<function>`, Atom — `#<atom>`

#### Scenario: Чтение текстового файла
- **WHEN** `(slurp "notes.txt")` исполняется из рабочего каталога, где `notes.txt` доступен для чтения и содержит несколько строк
- **THEN** функция разрешает `notes.txt` относительно этого рабочего каталога
- **AND** возвращает строку с полным содержимым файла, включая переводы строк

#### Scenario: Неверные аргументы slurp
- **WHEN** `slurp` получает не ровно одну строку
- **THEN** evaluation вызывает `Eval_error` с `slurp expects one path`

#### Scenario: Ошибка чтения файла
- **WHEN** `slurp` не может открыть или прочитать файл по `PATH`
- **THEN** evaluation вызывает `Eval_error` с `slurp failed: PATH`

#### Scenario: Arithmetic
- **WHEN** арифметические stdlib functions получают допустимые целые числа
- **THEN** возвращаются числовые значения
- **AND** `/` использует целочисленное деление
- **AND** +, -, * поддерживают смешанные числа и нормализацию согласно fractional-arithmetic; числовые строки не принимаются

#### Scenario: Функции Atom доступны без импорта
- **WHEN** программа вызывает `atom`, `deref`, `reset!` или `swap!` без пользовательских определений этих имён
- **THEN** evaluator разрешает их через stdlib и выполняет согласно capability `atoms`
