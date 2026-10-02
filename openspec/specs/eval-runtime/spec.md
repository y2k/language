# Eval Runtime Spec

## Purpose

Describe the current interpreter target, runtime values, core eval forms, and eval stdlib behavior.

## Requirements

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

### Requirement: Evaluator SHALL treat cast as a transparent core form

Evaluator SHALL обработать `(cast TYPE value)` как core-форму, которая игнорирует `TYPE`, вычисляет `value` ровно один раз и возвращает полученное значение без проверки типа.

#### Scenario: Evaluate cast
- **WHEN** evaluated source содержит `(cast java.util.List (str "a" "b"))`
- **THEN** результатом является `ab`
- **AND** evaluator не пытается разрешить символ `java.util.List`

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

### Requirement: Evaluator SHALL предоставлять get-in с путём-вектором

Evaluator SHALL поддерживать `(get-in collection keys)` с двумя аргументами и путём-вектором, в том числе полученным из переменной или функции. Результат SHALL соответствовать последовательному двухаргументному get, начиная с collection. Путь SHALL поддерживать map keys и неотрицательные целочисленные индексы, включая смешанные пути. Пустой путь SHALL возвращать исходное значение. Отсутствующий ключ, индекс за границей или промежуточный nil SHALL давать nil; конечный false SHALL сохраняться. Продолжение пути через скаляр SHALL завершаться Eval_error. Тип пути SHALL проверяться даже при исходной nil: допускается общий runtime-тип list/vector, но не nil, map или скаляр. При обращении к list отрицательный, строковый, символьный или binary64-индекс SHALL отклоняться с Eval_error. Трёхаргументный вариант SHALL отклоняться.

#### Scenario: Вложенный ключ и смешанный путь
- **WHEN** исполняются `(get-in {:chat {:id 42}} [:chat :id])` и `(get-in {:items [{:id 42}]} [:items 0 :id])`
- **THEN** оба результата — 42

#### Scenario: Путь из значения
- **WHEN** исполняется `(let [path [:chat :id]] (get-in {:chat {:id 42}} path))`
- **THEN** результат — 42

#### Scenario: Отсутствующие данные
- **WHEN** исполняются `(get-in {} [:chat :id])`, `(get-in {:chat nil} [:chat :id])`, `(get-in nil [:chat :id])` и `(get-in {:items []} [:items 0 :id])`
- **THEN** каждый результат — nil

#### Scenario: Сохранение false
- **WHEN** исполняется `(get-in {:enabled false} [:enabled])`
- **THEN** результат — boolean false

#### Scenario: Пустой путь
- **WHEN** исполняются `(get-in {:id 42} [])`, `(get-in nil [])` и `(get-in false [])`
- **THEN** возвращается исходное значение

#### Scenario: Скаляр внутри пути
- **WHEN** выполняется `(get-in {:chat value} [:chat :id])`, где value — число, строка, символ или boolean
- **THEN** возникает Eval_error

#### Scenario: Неверный тип пути
- **WHEN** get-in получает вместо пути nil, map либо скаляр, в том числе при исходном nil
- **THEN** возникает Eval_error

#### Scenario: Неверный индекс внутри пути
- **WHEN** выполняется `(get-in {:items [10]} [:items index])`, где index — -1, `"0"` или 0.0
- **THEN** возникает Eval_error с причиной неверного list index
- **WHEN** путь проходит через nil вместо списка
- **THEN** возвращается nil без попытки трактовать следующий ключ как list index

### Requirement: Unknown eval behavior SHALL remain unspecified

Спецификация SHALL оставлять неопределённым поведение вне принятых контрактов. Отрицательные list indexes и деление на ноль в eval SHALL давать определённые Eval_error; переносимость этих ошибок на другие targets не обещается. Non-finite числа, переполнение и nested def SHALL оставаться вне гарантий.

#### Scenario: Nested `def`
- **WHEN** def появляется вне соглашения о top-level формах
- **THEN** поведение остаётся неопределённым и программа не должна на него полагаться

#### Scenario: Decimal arithmetic
- **WHEN** дробные числа используются в +, - или * в пределах fractional-arithmetic
- **THEN** действует этот контракт
- **WHEN** binary64-значения передаются / либо операциям порядка в eval
- **THEN** возникает Eval_error из-за неверного типа
- **AND** общий переносимый контракт дробного деления/порядка этим не вводится

#### Scenario: Negative list indexes
- **WHEN** get обращается к list по отрицательному целочисленному индексу
- **THEN** возникает Eval_error с именем операции и причиной недопустимого индекса

#### Scenario: Division by zero
- **WHEN** / выполняет шаг целочисленного деления с нулевым делителем
- **THEN** возникает Eval_error с именем операции и причиной деления на ноль

### Requirement: Eval SHALL сохранять отдельный boolean во всех операциях

Boolean SHALL быть равен только boolean с тем же значением. `=`, `not=`, `not`, `vector?`, операции порядка и успешный `assert` SHALL возвращать boolean. `not` и `assert` SHALL использовать истинность `if`. Передача через функции, коллекции, деструктуризацию и Atom SHALL сохранять boolean. `str` SHALL преобразовывать boolean в обычный истинный текст `true`/`false`, включая внутри коллекций.

#### Scenario: Различие boolean и текста
- **WHEN** вычисляются `(= false "false")`, `(not= true "true")`, `(= [false] ["false"])` и `(= {:x false} {:x "false"})`
- **THEN** результаты — false, true, false и false

#### Scenario: Результаты предикатов
- **WHEN** вычисляются `(= (not true) false)`, `(= (vector? []) true)`, `(= (< 1 2) true)` и `(= (assert "false") true)`
- **THEN** каждый результат — true
- **WHEN** вычисляется `(assert false)` или `(assert nil)`
- **THEN** возникает `Eval_error` с `assertion failed`

#### Scenario: Boolean и текст как ключи
- **WHEN** m равно `(hash-map false 1 "false" 2 true 3 "true" 4)`
- **THEN** get по этим четырём ключам возвращает соответственно 1, 2, 3, 4
- **AND** ассоциативная деструктуризация по тем же ключам в let и параметре функции даёт те же значения

#### Scenario: Передача boolean
- **WHEN** false проходит через identity-функцию, list, get-in, деструктуризацию или atom/deref/reset!/swap!
- **THEN** результат остаётся boolean false, ложным и не равным `"false"`

#### Scenario: Текстовое представление boolean
- **WHEN** вычисляются `(str false)`, `(str [false true])`, `(if (str false) 1 2)` и `(not "false")`
- **THEN** результаты — `"false"`, `"(false true)"`, 1 и false

#### Scenario: Диагностика boolean
- **WHEN** boolean вызывается как функция, например `(false)`
- **THEN** eval сообщает ошибку вызова с указанием boolean-значения, не называя его текстовым symbol

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

### Requirement: Eval stdlib SHALL проверять аргументы по явной матрице контрактов

Eval SHALL обеспечивать следующие арности и типы уже вычисленных аргументов. Неверная арность либо тип SHALL давать Eval_error; автоматический разбор строки/символа вместо числа SHALL NOT выполняться. «Целое» означает целочисленный тип, а не binary64-значение с нулевой дробной частью. «Функция» означает callable-значение языка. Таблица SHALL применяться ко всем перечисленным bindings независимо от размера коллекций.

| Binding | Арность | Допустимые аргументы и границы |
| --- | --- | --- |
| list | 0+ | Любые значения |
| hash-map | Чётная, включая 0 | Пары любых значений; порядок и дубликаты сохраняются |
| str | 0+ | Любые значения; результат — строка |
| concat | 0+ | Только lists; результат — list |
| =, not= | 0+ | Любые значения; not= отрицает = |
| not, assert, vector? | 1 | Любое значение; assert дополнительно требует истинность |
| atom | 1 | Любое значение |
| deref | 1 | Atom |
| reset! | 2 | Atom и любое новое значение |
| swap! | 2 | Atom и функция |
| count | 1 | List либо map; результат — целое |
| slurp | 1 | Строковый путь |
| get | 2 | Map и любой ключ; list и неотрицательное целое; nil и любой ключ |
| get-in | 2 | Произвольное исходное значение и list/vector пути; непустой путь подчиняется get |
| map | 2 | Функция и list |
| reduce | 2 или 3 | Функция, необязательный init любого типа и list/map; без init коллекция непустая |
| drop | 2 | Целое count и list; count <= 0 сохраняет list |
| +, * | 0+ | Числа, включая смешанные целые/binary64 |
| - | 1+ | Числа, включая смешанные целые/binary64; unary не меняет знак |
| / | 1+ | Целые числа; каждый делитель ненулевой; unary возвращает аргумент |
| >, <, >=, <= | 2 | Два целых числа |

#### Scenario: Сохранение пустых и unary-вызовов
- **WHEN** вычисляются `(+)`, `(*)`, `(- 2)`, `(/ 2)`, `(str)`, `(list)` и `(concat)`
- **THEN** результаты — 0, 1, 2, 2, пустая строка, пустой list и пустой list
- **WHEN** вычисляются `(=)`, `(= 1)`, `(not=)` и `(not= 1)`
- **THEN** результаты — true, true, false и false

#### Scenario: Неверные арности
- **WHEN** вызываются `(not)`, `(count [] [])`, `(get {})`, `(get {} :x nil)`, `(get-in {} [] nil)`, `(map (fn [x] x))`, `(reduce)`, `(-)`, `(/)` или `(< 1 2 3)`
- **THEN** каждый вызов завершается Eval_error, называющим операцию и ожидаемые аргументы

#### Scenario: Неверные типы
- **WHEN** вызываются `(+ "1" 2)`, `(* true 2)`, `(count "abc")`, `(concat [] {})`, `(drop 1.0 [])`, `(/ 1.0 1)`, `(> 1.0 0)`, `(slurp 42)` или `(deref false)`
- **THEN** каждый вызов завершается Eval_error соответствующей операции

#### Scenario: Неотрицательные индексы и отрицательный drop
- **WHEN** вычисляются `(get [10] 5)`, `(get [] 0)`, `(drop -1 [10])` и `(drop 5 [10])`
- **THEN** результаты — nil, nil, list с 10 и пустой list
- **WHEN** выполняется `(get [10] -1)` или `(get [] -1)`
- **THEN** возникает Eval_error, а не необработанное OCaml-исключение

#### Scenario: Целочисленное деление
- **WHEN** вычисляются `(/ 7 2)`, `(/ -7 2)`, `(/ 20 2 2)` и `(/ 0)`
- **THEN** результаты — 3, -3, 5 и 0
- **WHEN** вычисляется `(/ 1 0)` либо `(/ 20 2 0)`
- **THEN** возникает Eval_error о делении на ноль

### Requirement: Eval SHALL проверять callable до обхода коллекции

Map, reduce и swap! SHALL отклонять не-callable аргумент независимо от того, будет ли он фактически вызван. При допустимом callable проверка арности его параметров SHALL происходить при вызове, без предварительного исполнения callback. Ошибки callback SHALL сохраняться; eval SHALL NOT заменять их общей ошибкой внешнего builtin. При ошибке swap! SHALL NOT выполнять финальную запись результата, но побочные эффекты callback не откатываются.

#### Scenario: Неверный callback без итераций
- **WHEN** выполняются `(map 42 [])`, `(reduce 42 0 [])`, `(reduce 42 [1])` и `(swap! (atom 1) 42)`
- **THEN** каждый вызов завершается Eval_error, называющим внешний builtin и ожидаемую функцию

#### Scenario: Допустимый callback без вызовов
- **WHEN** map получает функцию и пустой list, reduce получает функцию/init/пустую коллекцию либо функцию/одноэлементную коллекцию
- **THEN** callback не вызывается; результат — пустой list, init либо единственный элемент соответственно

#### Scenario: Ошибка внутри callback
- **WHEN** callback в map, reduce либо swap! вызывает `(assert false)`
- **THEN** наружу передаётся `Eval_error` с `assertion failed`
- **AND** swap! не записывает результат неуспешного callback

### Requirement: Eval SHALL использовать единое равенство ключей при чтении коллекций

Get, get-in и ассоциативная деструктуризация SHALL использовать равенство =, введённое типизированными скалярами. Lookup SHALL возвращать первую соответствующую пару, не удаляя дубликаты. Изменение формы доступа SHALL NOT менять результат, смешивать строку с числом либо boolean или вызывать host structural comparison функций.

#### Scenario: Эквивалентные числовые ключи
- **WHEN** m равно `(hash-map 1 "first" 1.0 "second" "1" "text")`
- **THEN** `(get m 1.0)`, `(get-in m [1])` и извлечение ключа 1.0 деструктуризацией возвращают `"first"`
- **AND** get по строке `"1"` возвращает `"text"`, count m равен 3

#### Scenario: Несовпадающие и callable-ключи
- **WHEN** в map присутствуют ключи false, `"false"`, функция и Atom
- **THEN** boolean и строка находятся независимо, а функция и Atom не совпадают согласно = и дают nil без host-исключения

### Requirement: Eval SHALL сообщать ожидаемые ошибки stdlib через Eval_error

Ошибки аргументов SHALL содержать имя builtin и ожидаемую арность или категорию аргумента. Ошибки отрицательного индекса и деления на ноль SHALL содержать имя операции и причину. Ошибки файлового API slurp, включая недопустимый путь, SHALL преобразовываться в Eval_error на границе операции. Существующие сообщения `assertion failed`, `hash-map arguments must be key/value pairs`, `slurp expects one path` и `slurp failed: PATH` SHALL сохраняться. Точный текст остальных ошибок не фиксируется. Runner SHALL преобразовывать Eval_error в error result; CLI SHALL сообщать ошибку в stderr с ненулевым статусом.

#### Scenario: Ошибки доходят до runner
- **WHEN** через eval runner выполняется вызов с неверной арностью, неверным типом, отрицательным индексом либо делением на ноль
- **THEN** runner возвращает ошибку языка вместо успешного scalar result или необработанного host-исключения

#### Scenario: Ошибка файловой операции
- **WHEN** slurp получает строковый путь к отсутствующему файлу либо путь с недопустимым NUL-символом
- **THEN** возникает Eval_error с сообщением `slurp failed: PATH`

#### Scenario: Сохранение точных существующих сообщений
- **WHEN** выполняются `(assert false)`, `(hash-map :x)` и `(slurp 42)`
- **THEN** сообщения — соответственно `assertion failed`, `hash-map arguments must be key/value pairs` и `slurp expects one path`

### Requirement: Eval SHALL предоставлять компиляцию regex из строки

Eval SHALL предоставлять binding `(re-pattern pattern)` без импорта, принимающий ровно одну строку и возвращающий отдельное скомпилированное regex-значение. Диалект SHALL быть документированным Perl-style подмножеством: литералы, классы символов, альтернативы, группы, greedy/lazy quantifiers и anchors; полная совместимость с Perl, PCRE или Java Pattern не обещается. По умолчанию поиск SHALL учитывать регистр, точка SHALL NOT совпадать с переводом строки, `^` и `$` SHALL обозначать начало и конец всей строки. Строки SHALL обрабатываться побайтово без Unicode-нормализации. API SHALL NOT принимать дополнительный аргумент flags. Inline flags, lookaround и backreferences SHALL отклоняться как недопустимый либо неподдерживаемый шаблон. Пустой шаблон SHALL быть допустимым. Нового reader-синтаксиса regex не вводится.

#### Scenario: Компиляция и повторное использование
- **WHEN** r связано с `(re-pattern "foo[0-9]+")` и передаётся в re-find для строк `"xfoo42"` и `"foo7"`
- **THEN** результаты — `"foo42"` и `"foo7"`; оба вызова используют одно regex-значение

#### Scenario: Настройки поиска по умолчанию
- **WHEN** re-find ищет шаблоны `"foo"`, `"^foo"` и `"a.b"` в строках `"FOO"`, `"x\nfoo"` и `"a\nb"` соответственно
- **THEN** все результаты — nil
- **WHEN** re-find ищет `"foo$"` в строках `"foo"` и `"foo\n"`
- **THEN** результаты — `"foo"` и nil соответственно

#### Scenario: Недопустимые шаблоны
- **WHEN** re-pattern получает незакрытый класс `"["`, inline flag `"(?m)^foo"`, lookahead `"(?=foo)"` либо шаблон с backreference
- **THEN** возникает Eval_error с именем re-pattern и причиной некорректного или неподдерживаемого шаблона, а не host-исключение

#### Scenario: Отрицательный word class внутри скобок
- **WHEN** re-find ищет шаблоны `"[\\W]+"`, `"\\W+"` и `"[^\\w]+"` в строках `"!!!"` и `"abc_42"`
- **THEN** каждый шаблон возвращает `"!!!"` для первой строки и nil для второй
- **WHEN** re-replace заменяет совпадения этих шаблонов в `"abc_42!!!"` строкой `"-"`
- **THEN** каждый результат — `"abc_42-"`
- **WHEN** re-find ищет `"[^\\W]+"` в `"!!!abc_42"` и `"[\\W_]+"` в `"a_!?b"`
- **THEN** результаты — `"abc_42"` и `"_!?"` соответственно

### Requirement: Eval SHALL сохранять regex как непрозрачное first-class значение

Regex SHALL отличаться от строки, символа, коллекции и функции и сохраняться при передаче через bindings, функции, коллекции, деструктуризацию и Atom. Regex SHALL быть истинным и не-callable. Для двух и более аргументов равенства regex SHALL не совпадать ни с каким значением, включая себя, аналогично функциям и Atom; правила нулевой/одиночной арности =/not= сохраняются. Это SHALL применяться рекурсивно в коллекциях и при lookup ключей без host structural comparison. `str` SHALL выводить regex как `#<regex>`, в том числе внутри коллекций. Финальный regex SHALL давать пустую строку результата runner; явный str SHALL давать текстовый результат. Ошибка прямого вызова regex SHALL называть категорию regex.

#### Scenario: Передача и истинность
- **WHEN** r проходит через identity-функцию, get/get-in, list/map-деструктуризацию в let или fn, atom/deref/reset!/swap!
- **THEN** полученное значение остаётся пригодным для re-find и истинным
- **AND** отсутствующий элемент по-прежнему даёт nil; цитированный `(re-pattern "x")` остаётся данными и не создаёт regex

#### Scenario: Равенство и ключи
- **WHEN** r равно `(re-pattern "x")` и вычисляются `(= r r)`, `(not= r r)`, `(= [r] [r])`, `(= r "x")`, `(= r)` и `(not= r)`
- **THEN** результаты — false, true, false, false, true и false
- **WHEN** выполняются `(get (hash-map r 1) r)` и `(get-in (hash-map r 1) [r])`
- **THEN** оба результата — nil без host-исключения

#### Scenario: Текст и диагностика
- **WHEN** вычисляются `(str (re-pattern "x"))` и `(str [(re-pattern "x")])`
- **THEN** результаты — `"#<regex>"` и `"(#<regex>)"`
- **WHEN** финальное выражение runner — `(re-pattern "x")`
- **THEN** успешный результат runner — пустая строка
- **WHEN** regex вызывается как функция
- **THEN** Eval_error указывает категорию regex

### Requirement: Eval SHALL возвращать первое полное regex-совпадение

Binding `(re-find regex text)` SHALL принимать ровно regex и строку в указанном порядке. Поиск SHALL возвращать первое слева полное совпадение строкой, независимо от наличия capture groups, либо отдельное nil при отсутствии совпадений. Greedy/lazy и порядок альтернатив SHALL определять выбор совпадения в одной позиции. Пустое совпадение SHALL возвращаться как пустая истинная строка, не nil. Передача строки вместо compiled regex SHALL NOT вызывать неявную компиляцию.

#### Scenario: Первое совпадение и группы
- **WHEN** выполняются `(re-find (re-pattern "foo([0-9]+)") "xfoo42 foo7")` и `(re-find (re-pattern "a|ab") "ab")`
- **THEN** результаты — `"foo42"` и `"a"`, не коллекция групп
- **WHEN** выполняются `(re-find (re-pattern "a+") "aaa")` и `(re-find (re-pattern "a+?") "aaa")`
- **THEN** результаты — `"aaa"` и `"a"`

#### Scenario: Отсутствующее и пустое совпадение
- **WHEN** выполняется `(re-find (re-pattern "x") "abc")`
- **THEN** результат равен nil, ложен и не равен `"nil"`
- **WHEN** выполняется `(re-find (re-pattern "") "abc")` либо `(re-find (re-pattern "") "")`
- **THEN** результат равен пустой строке, истинен и не равен nil

### Requirement: Eval SHALL выполнять глобальную буквальную regex-замену

Binding `(re-replace text regex replacement)` SHALL принимать ровно строку, compiled regex и строку замены в указанном порядке. Результат SHALL быть строкой с заменой всех неперекрывающихся совпадений слева направо. Замена SHALL вставляться буквально: dollar/backslash не раскрывают группы, callback не поддерживается, вставленный текст не сканируется повторно. При отсутствии совпадений SHALL возвращаться исходное содержимое. Пустая строка замены SHALL удалять совпадение.

При пустом совпадении SHALL вставляться replacement и поиск продолжаться после следующего исходного байта с сохранением этого байта; пустое совпадение в конце SHALL обрабатываться не более одного раза. Пустое совпадение непосредственно после непустого в той же позиции SHALL пропускаться с продвижением по исходному байту, если он есть. Обход SHALL завершаться для пустых шаблонов и строк.

#### Scenario: Несколько замен и отсутствие совпадений
- **WHEN** выполняются `(re-replace "foo1 foo22" (re-pattern "foo[0-9]+") "X")`, `(re-replace "abc" (re-pattern "x") "X")` и `(re-replace "export function f() {}" (re-pattern "^export ") "")`
- **THEN** результаты — `"X X"`, `"abc"` и `"function f() {}"`

#### Scenario: Буквальная замена без повторного поиска
- **WHEN** шаблон с capture group совпадает, а replacement содержит dollar или backslash
- **THEN** replacement вставляется побайтово без подстановки групп
- **WHEN** выполняются `(re-replace "aa" (re-pattern "a") "aa")` и `(re-replace "aaa" (re-pattern "aa") "X")`
- **THEN** результаты — `"aaaa"` и `"Xa"`

#### Scenario: Пустые совпадения
- **WHEN** выполняются `(re-replace "ab" (re-pattern "") "-")`, `(re-replace "" (re-pattern "") "-")` и `(re-replace "ab" (re-pattern "a*") "-")`
- **THEN** результаты — `"-a-b-"`, `"-"` и `"-b-"`; каждый вызов завершается

#### Scenario: Сборка userscript
- **WHEN** eval-программа читает header, runtime и compiled JS через slurp, извлекает текст regex-поиском, удаляет выбранные module directives через re-replace и объединяет части через str
- **THEN** финальная строка содержит ожидаемый standalone script в заданном порядке и выводится CLI для shell redirection
- **AND** regex API не требует sed, Python либо JS bundler для этих текстовых операций

### Requirement: Eval SHALL проверять контракты regex API и сообщать ошибки языка

Арности re-pattern/re-find/re-replace SHALL быть соответственно 1/2/3, типы — String, Regex+String, String+Regex+String. Другие арности и типы SHALL давать Eval_error с именем операции и ожидаемыми аргументами, даже для пустого text или отсутствующего совпадения. Строками считаются существующие строковые runtime-значения, включая keywords; числа, символы, nil и boolean не преобразуются в строки. Ошибки шаблона SHALL давать Eval_error с причиной; точное диагностическое сообщение не фиксируется. Runner SHALL возвращать Error, CLI SHALL выводить диагностику в stderr и завершаться ненулевым кодом. Эти гарантии SHALL относиться к eval; контракт JS/Java не расширяется.

#### Scenario: Неверные арности и типы
- **WHEN** выполняются `(re-pattern)`, `(re-pattern "x" "flags")`, `(re-pattern 'x)`, `(re-find "x" "")`, `(re-find (re-pattern "x") nil)` либо `(re-replace "" (re-pattern "x") (fn [x] x))`
- **THEN** каждый вызов даёт Eval_error, называющий соответствующий builtin и ожидаемые аргументы

#### Scenario: Ошибки через runner и CLI
- **WHEN** через runner и CLI запускается regex-вызов с неверной арностью, типом, ошибочным либо неподдерживаемым шаблоном
- **THEN** runner возвращает Error, CLI пишет его сообщение в stderr и завершается ненулевым кодом без успешного вывода

### Requirement: Eval SHALL предоставлять run! для последовательных эффектов

Eval SHALL предоставлять без импорта обычный callable binding `(run! f items)` для функции и list/vector. Run! SHALL немедленно вызывать callback с одним аргументом для каждого элемента в исходном порядке ровно один раз, игнорировать результаты и SHALL NOT собирать результирующую коллекцию. После успешного обхода SHALL возвращаться отдельное nil, не строка. Пустая коллекция SHALL NOT вызывать callback и SHALL возвращать nil. Поддержка map, nil как коллекции и иных iterable этим требованием не вводится. Ошибка callback SHALL прерывать обход и передаваться наружу без замены общей ошибкой run!; выполненные эффекты SHALL NOT откатываться.

#### Scenario: Порядок и немедленное завершение
- **WHEN** `(run! f [1 2 3])` записывает аргументы callback в Atom и callback возвращает произвольные значения
- **THEN** сразу после возвращения run! журнал содержит ровно 1, 2, 3 в этом порядке
- **AND** результат равен nil, ложен и не равен строке `"nil"`

#### Scenario: Пустой обход
- **WHEN** `(run! f [])` получает функцию с наблюдаемым побочным эффектом
- **THEN** функция не вызывается, эффект отсутствует и результат равен nil

#### Scenario: Именованная функция и значение функции
- **WHEN** callback передаётся как именованная функция, лямбда либо локальное функциональное значение
- **THEN** run! выполняет один и тот же последовательный обход

#### Scenario: Ошибка callback
- **WHEN** callback записывает эффект и затем вызывает `(assert false)` на втором элементе из трёх
- **THEN** наружу передаётся `Eval_error` с `assertion failed`, третий элемент не обрабатывается
- **AND** эффекты первого и второго вызовов сохраняются

### Requirement: Eval SHALL проверять аргументы run! по образцу map

Run! SHALL принимать ровно два аргумента: callable и list/vector. Неверная арность, не-callable callback или неверный тип коллекции SHALL давать Eval_error с именем run! и ожидаемыми аргументами. Callable SHALL проверяться до обхода, включая пустую коллекцию. Арность параметров допустимого callback SHALL проверяться существующим механизмом только при фактическом вызове, без предварительного исполнения callback.

#### Scenario: Неверные аргументы
- **WHEN** выполняются `(run!)`, `(run! (fn [x] x))`, `(run! (fn [x] x) [] nil)`, `(run! 42 [])`, `(run! (fn [x] x) {})` или `(run! (fn [x] x) nil)`
- **THEN** каждый вызов даёт Eval_error, называющий run! и ожидаемые аргументы

#### Scenario: Арность callback при фактическом вызове
- **WHEN** `(run! (fn [a b] a) [])` получает пустую коллекцию
- **THEN** callback не вызывается и результат равен nil
- **WHEN** та же функция передаётся run! с непустой коллекцией
- **THEN** ошибка арности возникает при вызове callback
