## MODIFIED Requirements

### Requirement: The eval stdlib SHALL provide the implemented functions

Stdlib SHALL предоставлять bindings `list`, `=`, `not=`, `not`, `>`, `<`, `>=`, `<=`, `vector?`, `concat`, `hash-map`, `get`, `get-in`, `str`, `slurp`, `count`, `map`, `reduce`, `drop`, `+`, `-`, `*` и `/`. Этот перечень SHALL NOT исключать bindings, заданные другими требованиями. `slurp` SHALL разрешать relative path от текущего рабочего каталога процесса.

#### Scenario: Lists and hash maps
- **WHEN** `list` вызывается с произвольными аргументами
- **THEN** результатом является runtime list этих аргументов в исходном порядке
- **WHEN** `hash-map` вызывается с чётным числом аргументов
- **THEN** результатом является runtime hash map с парами ключ/значение в порядке аргументов
- **WHEN** `hash-map` вызывается с нечётным числом аргументов
- **THEN** возникает `Eval_error` с `hash-map arguments must be key/value pairs`

#### Scenario: Equality
- **WHEN** `=` получает ноль или одно значение
- **THEN** результат равен `true`
- **WHEN** `=` получает несколько значений
- **THEN** результат равен `true` только при структурном равенстве всех runtime values

#### Scenario: Two-argument scalar inequality
- **WHEN** `not=` получает ровно два значения `nil`, boolean, string или integer, поддерживаемые `=`
- **THEN** результат равен логическому отрицанию результата `=` для этих значений

#### Scenario: Logical negation
- **WHEN** `not` получает `false` или `nil`
- **THEN** результат равен `true`
- **WHEN** `not` получает любое другое runtime value
- **THEN** результат равен `false`

#### Scenario: Integer comparisons
- **WHEN** `>` или `<` получает два integer symbol values, представимых evaluator
- **THEN** результат равен `true` ровно когда первое значение соответственно больше или меньше второго
- **WHEN** `>=` или `<=` получает два таких значения
- **THEN** результат равен `true` ровно когда первое значение соответственно больше либо равно или меньше либо равно второму

#### Scenario: Count collections
- **WHEN** `count` получает один list или hash map
- **THEN** результатом является число элементов или пар как symbol value

#### Scenario: Concatenate lists
- **WHEN** `concat` получает только lists
- **THEN** результатом является один runtime list со всеми элементами в исходном порядке

#### Scenario: Vector predicate
- **WHEN** `vector?` получает runtime list
- **THEN** результат равен `true`
- **WHEN** `vector?` получает одно значение другого типа
- **THEN** результат равен `false`

#### Scenario: Get from collections
- **WHEN** `get` получает hash map и ключ
- **THEN** результатом является связанное значение или `nil`
- **WHEN** `get` получает list и неотрицательный целый индекс
- **THEN** результатом является элемент по индексу или `nil`

#### Scenario: Get from nil
- **WHEN** исполняется `(get nil :text)` или `(:text nil)`
- **THEN** результат равен `nil`

#### Scenario: Map and reduce
- **WHEN** `map` получает функцию и list
- **THEN** результатом является list результатов вызова функции для каждого элемента
- **WHEN** `reduce` получает функцию и непустую коллекцию без init
- **THEN** свёртка начинается с первого элемента
- **WHEN** `reduce` получает функцию, init и коллекцию
- **THEN** каждый элемент сворачивается начиная с init

#### Scenario: Drop items
- **WHEN** `drop` получает целое число и list
- **THEN** результатом является list без первых указанного числа элементов
- **AND** число меньше либо равное нулю возвращает исходный list

#### Scenario: String conversion
- **WHEN** `str` получает runtime values
- **THEN** результатом является один symbol value: symbols представлены своим текстом, lists — строками элементов в круглых скобках, hash maps — парами ключ/значение в фигурных скобках в сохранённом порядке, closures — `#<function>`

#### Scenario: Чтение текстового файла
- **WHEN** `(slurp "notes.txt")` исполняется из рабочего каталога, где файл доступен и содержит несколько строк
- **THEN** путь разрешается относительно этого рабочего каталога
- **AND** результат содержит полный текст файла, включая переводы строк, как symbol value

#### Scenario: Неверные аргументы slurp
- **WHEN** `slurp` получает не ровно один symbol value
- **THEN** возникает `Eval_error` с `slurp expects one path`

#### Scenario: Ошибка чтения файла
- **WHEN** `slurp` не может открыть или прочитать файл по `PATH`
- **THEN** возникает `Eval_error` с `slurp failed: PATH`

#### Scenario: Arithmetic
- **WHEN** арифметические stdlib functions получают integer symbol values
- **THEN** результатом являются integer symbol values
- **AND** `/` выполняет целочисленное деление

## ADDED Requirements

### Requirement: Evaluator SHALL предоставлять get-in с путём-вектором

Evaluator SHALL поддерживать `(get-in collection keys)` с двумя аргументами и путём-вектором, в том числе полученным из переменной или функции. Результат SHALL соответствовать последовательному двухаргументному `get`, начиная с `collection`. Путь SHALL поддерживать map keys и неотрицательные целые индексы, включая смешанные пути. Пустой путь SHALL возвращать исходное значение. Отсутствующий ключ, индекс за границей или промежуточный `nil` SHALL давать `nil`; конечный `false` SHALL сохраняться. Продолжение пути через число, строку или boolean SHALL завершаться ошибкой. `nil`, map, строка и скаляры вместо пути SHALL отклоняться, даже при исходной коллекции `nil`. Точный текст ошибок не фиксируется. Поведение list-пути и некорректных индексов не специфицируется; существующая поддержка list-пути допустима. Трёхаргументный вариант не входит в контракт.

#### Scenario: Вложенный ключ и смешанный путь
- **WHEN** исполняются `(get-in {:chat {:id 42}} [:chat :id])` и `(get-in {:items [{:id 42}]} [:items 0 :id])`
- **THEN** оба результата равны `42`

#### Scenario: Путь из значения
- **WHEN** исполняется `(let [path [:chat :id]] (get-in {:chat {:id 42}} path))`
- **THEN** результат равен `42`

#### Scenario: Отсутствующие данные
- **WHEN** исполняются `(get-in {} [:chat :id])`, `(get-in {:chat nil} [:chat :id])`, `(get-in nil [:chat :id])` и `(get-in {:items []} [:items 0 :id])`
- **THEN** каждый результат равен `nil`

#### Scenario: Сохранение false
- **WHEN** исполняется `(get-in {:enabled false} [:enabled])`
- **THEN** результат равен `false`

#### Scenario: Пустой путь
- **WHEN** исполняются `(get-in {:id 42} [])`, `(get-in nil [])` и `(get-in false [])`
- **THEN** результаты равны исходным значениям: map `{:id 42}`, `nil` и `false`

#### Scenario: Скаляр внутри пути
- **WHEN** исполняется `(get-in {:chat value} [:chat :id])`, где `value` равно `42`, `"hello"` или `false`
- **THEN** исполнение завершается ошибкой

#### Scenario: Неверный тип пути
- **WHEN** `get-in` получает вместо пути `nil`, map, строку, число или boolean, в том числе при исходной коллекции `nil`
- **THEN** исполнение завершается ошибкой
