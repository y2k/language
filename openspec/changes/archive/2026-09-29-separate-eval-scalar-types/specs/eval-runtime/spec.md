# Spec Delta

## MODIFIED Requirements

### Requirement: The eval target SHALL return only the final symbol value

Runner SHALL вычислять формы через eval и выводить только финальный scalar value: строку, символ, число, boolean либо nil. Строки и символы SHALL выводиться без кавычек, числа — по правилам `str`, boolean — как `true`/`false`, nil — как `nil`. Финальная коллекция, функция, Atom либо отсутствие результата SHALL давать пустую строку. Историческое имя требования не ограничивает перечисленные типы.

#### Scenario: Evaluate sample test function
- **WHEN** после тела fixture вычисляется `(test)`, возвращающий строку
- **THEN** runner возвращает её содержимое

#### Scenario: Non-symbol final result
- **WHEN** последний результат — list, hash map, closure, Atom либо результата нет
- **THEN** runner возвращает пустую строку

#### Scenario: Eval error result
- **WHEN** вычисление вызывает `Eval_error` с сообщением `MESSAGE`
- **THEN** runner возвращает `MESSAGE` как строку ошибки

#### Scenario: Финальное nil
- **WHEN** последнее выражение — `nil`, `'nil` либо `(if false 1)`
- **THEN** runner возвращает `nil`

#### Scenario: Финальный boolean
- **WHEN** последнее выражение — `false`, `true`, `(= 1 1)` либо `(not true)`
- **THEN** runner возвращает соответственно `false`, `true`, `true` либо `false`

#### Scenario: Финальные типизированные скаляры
- **WHEN** последнее выражение — `42`, `0.25`, `"text"` либо `'foo`
- **THEN** runner возвращает соответственно `42`, `0.25`, `text` либо `foo`

### Requirement: The evaluator SHALL resolve atoms by the implemented lookup order

Evaluator SHALL сначала распознавать строковые атомы в двойных кавычках как строки. Для остальных атомов SHALL проверяться locals, globals текущего namespace, stdlib, qualified references и литералы в этом порядке. Литералы SHALL давать отдельные nil, boolean и числовые значения, отличные от строк и символов. Числа SHALL поддерживать целое и binary64-представление без неявного преобразования строк. Keywords SHALL сохранять существующую семантику строк без ведущего двоеточия.

#### Scenario: String atom
- **WHEN** вычисляется `"abc"`, `"42"`, `"nil"` либо `"false"`
- **THEN** результат — строка с соответствующим содержимым, не число, nil или boolean

#### Scenario: Literal atom
- **WHEN** `nil`, `true`, `false`, `42` либо `0.25` не разрешены через bindings
- **THEN** результаты — nil, boolean true, boolean false, целое 42 и binary64-число 0.25

#### Scenario: Qualified name after stdlib lookup
- **WHEN** атом содержит `QUALIFIER/MEMBER`
- **THEN** qualifier разрешается через require aliases либо используется как namespace
- **AND** member ищется в globals этого namespace

#### Scenario: Missing symbol
- **WHEN** символ не удаётся разрешить
- **THEN** возникает `Eval_error` с `symbol not found: NAME`

#### Scenario: Литерал nil
- **WHEN** атом `nil` не разрешён предшествующими этапами поиска
- **THEN** результат — nil, не равный строке `"nil"`

#### Scenario: Keywords остаются строками
- **WHEN** вычисляются `(= :name "name")` и `(:name {:name 42})`
- **THEN** результаты — true и 42

### Requirement: The evaluator SHALL support core control and binding forms

Evaluator SHALL поддерживать top-level `def`, `compiler/ns`, `deps`, `quote`, `if`, `fn*`, `let*` и `do`. Bindings `let*` и параметры `fn*` SHALL поддерживать символы, последовательную и ассоциативную деструктуризацию и их вложенные сочетания. Каждый top-level шаблон параметра SHALL потреблять один аргумент. Только nil и boolean false SHALL быть ложными. Quote SHALL сохранять тип литералов; прочие атомы SHALL становиться символами без lookup. Ассоциативная деструктуризация SHALL использовать то же равенство ключей, что `get`.

#### Scenario: Define in current namespace
- **WHEN** вычисляется top-level `(def name value)`
- **THEN** value вычисляется и сохраняется в globals текущего namespace с исходным типом

#### Scenario: Compiler namespace form
- **WHEN** вычисляется `(compiler/ns "app" (("dep.ns" "alias")) ())`
- **THEN** текущим namespace становится app, alias разрешается в dep.ns
- **AND** результат — nil

#### Scenario: Quote form
- **WHEN** вычисляется `(quote value)`
- **THEN** nil, boolean, строки и числа сохраняют тип, остальные атомы становятся символами, списки цитируются рекурсивно без вычисления содержимого
- **AND** ранее выполненное раскрытие макросов внутри quote не меняется этим требованием

#### Scenario: If without else
- **WHEN** условие `(if condition then)` ложно
- **THEN** результат — nil

#### Scenario: Truthiness
- **WHEN** значение — nil либо boolean false
- **THEN** оно ложно
- **AND** строки, числа, символы и остальные значения истинны

#### Scenario: Lexical function closure
- **WHEN** `fn*` захватывает locals и позже вызывается
- **THEN** тело вычисляется с захваченными bindings и namespace замыкания

#### Scenario: Let bindings
- **WHEN** `let*` содержит пары имя/значение
- **THEN** значения вычисляются по порядку с доступом к предыдущим bindings, а тело — со всеми bindings

#### Scenario: Sequential destructuring let binding
- **WHEN** `(list a b)` связывается со списком из двух элементов
- **THEN** a и b получают соответствующие элементы с сохранением типов

#### Scenario: Associative destructuring let binding
- **WHEN** `(hash-map "name" n "age" a)` связывается с hash map
- **THEN** n и a получают значения соответствующих строковых ключей

#### Scenario: Missing associative destructuring key
- **WHEN** ключ шаблона отсутствует
- **THEN** соответствующий binding получает nil

#### Scenario: Nested destructuring let binding
- **WHEN** binding содержит вложенные шаблоны
- **THEN** имена связываются рекурсивным применением соответствующего шаблона

#### Scenario: Do body
- **WHEN** `do` содержит несколько выражений
- **THEN** они вычисляются по порядку и возвращается результат последнего

#### Scenario: Sequential destructuring function parameters
- **WHEN** параметр `(list a b)` получает список
- **THEN** его элементы связываются с a и b

#### Scenario: Associative and nested function parameters
- **WHEN** параметр `(hash-map "name" n "tags" (list first-tag))` получает соответствующую map
- **THEN** n и first-tag получают выбранные значения

#### Scenario: Missing function parameter values
- **WHEN** шаблон выбирает отсутствующий индекс либо ключ
- **THEN** соответствующий binding получает nil

#### Scenario: Extra function argument collection values
- **WHEN** список-аргумент длиннее последовательного шаблона
- **THEN** лишние элементы игнорируются

#### Scenario: Загрузка зависимостей
- **WHEN** deps успешно загружает зависимости, включая пустой набор
- **THEN** результат — nil
- **AND** имена пакетов и версии являются строками; `(deps {:package "version"})` сохраняет работоспособность

#### Scenario: Цитирование вложенного nil
- **WHEN** вычисляются `(= (get '(nil "nil") 0) nil)` и `(= (get '(nil "nil") 1) nil)`
- **THEN** результаты — true и false

#### Scenario: Цитирование boolean
- **WHEN** вычисляются `(= 'false false)`, `(= 'false "false")` и `(if (get '(false true) 0) 1 2)`
- **THEN** результаты — true, false и 2

#### Scenario: Выбранная ветка и управляющие макросы
- **WHEN** вычисляются `(if "false" 1 (assert false))`, `(and "false" 7)`, `(or false "false")` и `(if-let [x "false"] x "other")`
- **THEN** результаты — 1, 7, `"false"` и `"false"`; невыбранная ветка if не вычисляется

#### Scenario: Цитированные числа и символы
- **WHEN** вычисляются `(= '42 42)`, `(= '0.25 0.25)`, `(= 'foo 'foo)`, `(= 'foo "foo")` и `(= (get '(42 "42" foo) 1) 42)`
- **THEN** результаты — true, true, true, false и false

### Requirement: The eval stdlib SHALL provide the implemented functions

Stdlib SHALL предоставлять bindings `list`, `=`, `not=`, `not`, `assert`, `>`, `<`, `>=`, `<=`, `vector?`, `concat`, `hash-map`, `get`, `get-in`, `str`, `slurp`, `count`, `map`, `reduce`, `drop`, `+`, `-`, `*`, `/`, `atom`, `deref`, `reset!`, `swap!`. Этот перечень SHALL NOT исключать bindings других требований. Числовые операции SHALL принимать числовые значения, не текст; путь slurp SHALL быть строкой, не числом/символом. `slurp` SHALL разрешать relative path от cwd процесса. Типы SHALL сохраняться в коллекциях, функциях и Atom.

#### Scenario: Lists and hash maps
- **WHEN** list получает любые аргументы
- **THEN** возвращает list этих значений в исходном порядке
- **WHEN** hash-map получает чётное число аргументов
- **THEN** возвращает map с парами в исходном порядке
- **WHEN** hash-map получает нечётное число аргументов
- **THEN** возникает `Eval_error` с `hash-map arguments must be key/value pairs`

#### Scenario: Equality
- **WHEN** = получает ноль или один аргумент
- **THEN** возвращает boolean true
- **WHEN** = получает несколько значений
- **THEN** возвращает true только если все равны: числа сравниваются по значению, остальные скаляры — по типу и содержимому, lists/maps — рекурсивно с учётом порядка
- **AND** функции и Atom считаются неравными, включая сравнение с собой

#### Scenario: Two-argument scalar inequality
- **WHEN** not= получает два поддерживаемых скаляра
- **THEN** возвращает boolean, противоположный результату =

#### Scenario: Logical negation
- **WHEN** not получает nil либо boolean false
- **THEN** возвращает true
- **WHEN** not получает любое другое значение
- **THEN** возвращает false

#### Scenario: Integer comparisons
- **WHEN** >, <, >= или <= получает два целых числа
- **THEN** возвращает boolean соответствующего сравнения
- **AND** строки и binary64-аргументы, даже `1.0`, отклоняются с Eval_error

#### Scenario: Count collections
- **WHEN** count получает list либо map
- **THEN** возвращает целое число элементов либо пар, не строку

#### Scenario: Concatenate lists
- **WHEN** concat получает только lists
- **THEN** возвращает объединённый list с сохранением порядка элементов

#### Scenario: Vector predicate
- **WHEN** vector? получает list
- **THEN** возвращает true
- **WHEN** vector? получает одно значение иного типа
- **THEN** возвращает false

#### Scenario: Get from collections
- **WHEN** get получает map и ключ
- **THEN** возвращает значение первой пары с ключом, равным по правилам =, либо nil
- **WHEN** get получает list и неотрицательный целочисленный индекс
- **THEN** возвращает элемент либо nil при выходе за границу
- **AND** строковый, символьный или binary64-индекс отклоняется с Eval_error

#### Scenario: Get from nil
- **WHEN** вычисляется `(get nil :text)` либо `(:text nil)`
- **THEN** результат — nil

#### Scenario: Map and reduce
- **WHEN** map получает функцию и list
- **THEN** возвращает list результатов вызова функции для каждого элемента
- **WHEN** reduce получает функцию и непустую коллекцию без init
- **THEN** свёртка начинается с первого элемента
- **WHEN** reduce получает функцию, init и коллекцию
- **THEN** сворачивает все элементы начиная с init

#### Scenario: Drop items
- **WHEN** drop получает целое count и list
- **THEN** возвращает list без первых count элементов, при count <= 0 — исходный list
- **AND** строковые и binary64-count отклоняются с Eval_error

#### Scenario: String conversion
- **WHEN** str получает runtime-значения
- **THEN** возвращает строку, объединяя текст строк и символов без кавычек, чисел, boolean и nil; lists оформляются круглыми скобками, maps — фигурными с сохранением порядка пар, функции — `#<function>`, Atom — `#<atom>`

#### Scenario: Чтение текстового файла
- **WHEN** `(slurp "notes.txt")` выполняется из cwd с доступным многострочным файлом
- **THEN** путь разрешается относительно cwd и результатом является строка с полным содержимым, включая переводы строк

#### Scenario: Неверные аргументы slurp
- **WHEN** slurp получает не ровно одну строку
- **THEN** возникает `Eval_error` с `slurp expects one path`

#### Scenario: Ошибка чтения файла
- **WHEN** slurp не может открыть или прочитать файл PATH
- **THEN** возникает `Eval_error` с `slurp failed: PATH`

#### Scenario: Arithmetic
- **WHEN** арифметические функции получают допустимые целые числа
- **THEN** возвращают числовые значения, а / использует целочисленное деление
- **AND** +, -, * поддерживают смешанные числа и нормализацию согласно fractional-arithmetic; числовые строки не принимаются

#### Scenario: Функции Atom доступны без импорта
- **WHEN** программа вызывает atom, deref, reset! или swap! без пользовательских определений этих имён
- **THEN** evaluator разрешает stdlib bindings и сохраняет типы хранимых значений, aliasing и отсутствие финальной записи при ошибке swap!

## ADDED Requirements

### Requirement: Eval SHALL различать скалярные категории и сравнивать числа по значению

Nil, boolean, строки, символы и числа SHALL быть разными категориями равенства. Целые и binary64-числа SHALL сравниваться по числовому значению без epsilon; смешанное сравнение SHALL NOT терять точность целого через округление до binary64. Правило SHALL применяться рекурсивно в коллекциях и к поиску ключей. Гарантии относятся к конечным числам в поддерживаемом диапазоне.

#### Scenario: Разные категории
- **WHEN** вычисляются `(= 42 "42")`, `(= 'foo "foo")`, `(= false "false")` и `(= nil "nil")`
- **THEN** каждый результат — false

#### Scenario: Числовое равенство
- **WHEN** вычисляются `(= 1 1.0)`, `(not= 1 1.0)`, `(= 0 -0.0)` и `(= 0.25 0.250)`
- **THEN** результаты — true, false, true и true
- **WHEN** сравниваются binary64-значения с различным точным значением
- **THEN** = возвращает false без epsilon-сближения

#### Scenario: Большое целое не округляется при сравнении
- **WHEN** на eval с 64-bit OCaml сравниваются `9007199254740993` и `9007199254740992.0`
- **THEN** = возвращает false
- **WHEN** сравниваются `9007199254740992` и `9007199254740992.0`
- **THEN** = возвращает true

#### Scenario: Числа в коллекциях
- **WHEN** вычисляются `(= [1] [1.0])`, `(= [1] ["1"])` и `(= {:x 1} {:x 1.0})`
- **THEN** результаты — true, false и true

#### Scenario: Ключи map и первый результат
- **WHEN** m равно `(hash-map 1 "first" 1.0 "second" "1" "text" 'x "symbol" "x" "string")`
- **THEN** get по 1 и 1.0 возвращает `"first"`, по `"1"` — `"text"`, по 'x — `"symbol"`, по `"x"` — `"string"`
- **AND** count m остаётся 5; эквивалентные ключи не удаляются

#### Scenario: Деструктуризация и get-in используют то же равенство
- **WHEN** вычисляются `(let [{1 x} (hash-map 1.0 "ok")] x)`, `((fn [{1 x}] x) (hash-map 1.0 "ok"))` и `(get-in (hash-map 1.0 {:x "ok"}) [1 :x])`
- **THEN** все результаты — `"ok"`

#### Scenario: Функция и Atom как ключи
- **WHEN** ключ функции либо Atom ищется в map, в том числе той же самой ссылкой
- **THEN** совпадения нет согласно =, результат — nil, исключения структурного сравнения не возникает

#### Scenario: Типизированные producers и строгие consumers
- **WHEN** число возвращается арифметикой или count, а строка — str либо slurp
- **THEN** результат сохраняет соответствующий тип при передаче через функцию, коллекцию, деструктуризацию и Atom
- **WHEN** выполняются `(+ "1" 2)`, `(get [10] "0")`, `(get [10] 0.0)`, `(slurp 'file)` или `(deps {:package 1})`
- **THEN** возникает Eval_error из-за неподходящего типа

#### Scenario: Числовое равенство не расширяет integer-only API
- **WHEN** выполняется `(get [10 20] (+ 0.5 0.5))`
- **THEN** результат — 20, поскольку арифметический результат нормализован в целое
- **WHEN** выполняется `(get [10 20] 1.0)`
- **THEN** возникает Eval_error: binary64-литерал не является целочисленным индексом

#### Scenario: Диагностика различает скаляры
- **WHEN** строка, число или цитированный символ вызывается как функция
- **THEN** Eval_error указывает соответствующую категорию значения, а не называет все значения symbol
