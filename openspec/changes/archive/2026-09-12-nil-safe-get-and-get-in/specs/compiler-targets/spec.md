## MODIFIED Requirements

### Requirement: Compiler target runtimes SHALL provide core get

JavaScript и Java runtimes SHALL предоставлять двухаргументный `get` для list по неотрицательному целому индексу и hash-map по ключу. Отсутствующий ключ или индекс за границей SHALL возвращать `nil`/`null`. Коллекция `nil` SHALL возвращать `nil`/`null`. Чтение существующего `false` SHALL сохранять `false`. В JavaScript прочитанное `undefined` SHALL нормализоваться в `nil`/`null`. Числа, строки и booleans не получают поддержку как коллекции.

#### Scenario: Read list and hash-map values through get
- **WHEN** compiled source вызывает `get` для list по неотрицательному целому индексу или hash-map по ключу
- **THEN** runtime возвращает соответствующее значение
- **AND** отсутствующий индекс или ключ возвращает `nil`/`null`

#### Scenario: Nil-безопасный get и keyword lookup
- **WHEN** compiled source исполняет `(get nil :text)` или `(:text nil)`
- **THEN** JavaScript и Java возвращают `nil`/`null`

#### Scenario: Нормализация JS undefined
- **WHEN** JavaScript `get` читает отсутствующее свойство обычного JS object либо свойство со значением `undefined`
- **THEN** результат равен `null`
- **WHEN** прочитанное значение равно `false`, `0` или пустой строке
- **THEN** результат сохраняет это значение

## ADDED Requirements

### Requirement: Compiler targets SHALL предоставлять get-in с путём-вектором

JavaScript и Java SHALL поддерживать `(get-in collection keys)` с двумя аргументами и путём-вектором, в том числе полученным из переменной или функции. Результат SHALL соответствовать последовательному двухаргументному `get`, начиная с `collection`. Путь SHALL поддерживать map keys и неотрицательные целые индексы, включая смешанные пути. Пустой путь SHALL возвращать исходное значение. Отсутствующий ключ, индекс за границей или промежуточный `nil` SHALL давать `nil`; конечный `false` SHALL сохраняться. Продолжение пути через число, строку или boolean SHALL завершаться ошибкой. `nil`, map, строка и скаляры вместо пути SHALL отклоняться, даже при исходной коллекции `nil`. Точный текст ошибок не фиксируется. Поведение list-пути и некорректных индексов не специфицируется; существующая поддержка list-пути допустима. Трёхаргументный вариант не входит в контракт. Для гарантированных сценариев результаты SHALL совпадать с evaluator.

#### Scenario: Вложенный ключ и смешанный путь
- **WHEN** compiled source исполняет `(get-in {:chat {:id 42}} [:chat :id])` и `(get-in {:items [{:id 42}]} [:items 0 :id])`
- **THEN** оба результата равны `42` на JavaScript и Java

#### Scenario: Путь из значения
- **WHEN** compiled source исполняет `(let [path [:chat :id]] (get-in {:chat {:id 42}} path))`
- **THEN** результат равен `42`

#### Scenario: Отсутствующие данные
- **WHEN** compiled source исполняет `(get-in {} [:chat :id])`, `(get-in {:chat nil} [:chat :id])`, `(get-in nil [:chat :id])` и `(get-in {:items []} [:items 0 :id])`
- **THEN** каждый результат равен `nil`/`null`

#### Scenario: Сохранение false
- **WHEN** compiled source исполняет `(get-in {:enabled false} [:enabled])`
- **THEN** результат равен `false`

#### Scenario: Пустой путь
- **WHEN** compiled source исполняет `(get-in {:id 42} [])`, `(get-in nil [])` и `(get-in false [])`
- **THEN** результаты являются исходными значениями: map `{:id 42}`, `nil`/`null` и `false`

#### Scenario: Скаляр внутри пути
- **WHEN** compiled source исполняет `(get-in {:chat value} [:chat :id])`, где `value` равно `42`, `"hello"` или `false`
- **THEN** исполнение завершается ошибкой

#### Scenario: Неверный тип пути
- **WHEN** compiled source передаёт в `get-in` вместо пути `nil`, map, строку, число или boolean, в том числе при исходной коллекции `nil`
- **THEN** исполнение завершается ошибкой
