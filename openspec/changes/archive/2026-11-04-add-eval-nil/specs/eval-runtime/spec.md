# Spec Delta

## RENAMED Requirements

- FROM: `### Requirement: The eval target SHALL return only the final symbol value`
- TO: `### Requirement: The eval target SHALL return the final symbol or nil value`

## MODIFIED Requirements

### Requirement: The eval target SHALL return the final symbol or nil value

Runner SHALL вычислять формы через eval и возвращать только финальный scalar value: строку, символ, число, boolean либо nil. Строки и символы SHALL возвращаться без кавычек, числа — по правилам `str`, boolean — как `true`/`false`, nil — как `nil`. Финальная коллекция, функция, Atom либо отсутствие результата SHALL давать пустую строку. Побочный вывод println/eprintln SHALL выполняться отдельно и не входить в строку результата runner. CLI SHALL печатать результат runner в stdout с переводом строки. Историческое имя требования не ограничивает перечисленные типы.

#### Scenario: Evaluate sample test function
- **WHEN** после тела fixture вычисляется `(test)`, возвращающий строку
- **THEN** runner возвращает её содержимое

#### Scenario: Финальное nil
- **WHEN** последним выражением является `nil`, `'nil` или `(if false 1)`
- **THEN** runner возвращает строку `nil`

#### Scenario: Non-symbol final result
- **WHEN** последним результатом является список, hash map, closure, atom либо результата нет
- **THEN** runner возвращает пустую строку

#### Scenario: Eval error result
- **WHEN** вычисление вызывает `Eval_error` с сообщением `MESSAGE`
- **THEN** runner возвращает `MESSAGE` как строку ошибки

#### Scenario: Финальный boolean
- **WHEN** последнее выражение — `false`, `true`, `(= 1 1)` либо `(not true)`
- **THEN** runner возвращает соответственно `false`, `true`, `true` либо `false`

#### Scenario: Финальные типизированные скаляры
- **WHEN** последнее выражение — `42`, `0.25`, `"text"` либо `'foo`
- **THEN** runner возвращает соответственно `42`, `0.25`, `text` либо `foo`

#### Scenario: Диагностика и финальный результат CLI
- **WHEN** CLI с target eval исполняет только `(eprintln "hello")`
- **THEN** stderr равен `"hello\n"`, stdout равен `"nil\n"`, код завершения равен 0
- **WHEN** CLI исполняет только `(println "hello")`
- **THEN** stdout равен `"hello\nnil\n"`, stderr пуст, код завершения равен 0

#### Scenario: Вывод не заменяет полезный результат
- **WHEN** CLI с target eval исполняет `(do (println "out") (eprintln "err") "done")`
- **THEN** stdout равен `"out\ndone\n"`, stderr равен `"err\n"`, код завершения равен 0
- **AND** runner возвращает строку `done`, не объединённый журнал вывода

### Requirement: The evaluator SHALL resolve atoms by the implemented lookup order

Evaluator SHALL сначала распознавать строковые атомы в двойных кавычках как строки. Для остальных атомов SHALL проверяться locals, globals текущего namespace, stdlib, qualified references и литералы в этом порядке. Литералы SHALL давать отдельные nil, boolean и числовые значения, отличные от строк и символов. Числа SHALL поддерживать целое и binary64-представление без неявного преобразования строк. Keywords SHALL сохранять существующую семантику строк без ведущего двоеточия.

#### Scenario: String atom
- **WHEN** вычисляется `"abc"`, `"42"`, `"nil"` либо `"false"`
- **THEN** результат — строка с соответствующим содержимым, не число, nil или boolean

#### Scenario: Литерал nil
- **WHEN** атом `nil` не разрешён предшествующими этапами поиска
- **THEN** получается отдельное значение `nil`, не равное строке `"nil"`

#### Scenario: Literal atom
- **WHEN** `nil`, `true`, `false`, `42` либо `0.25` не разрешены через bindings
- **THEN** результаты — nil, boolean true, boolean false, целое 42 и binary64-число 0.25

#### Scenario: Qualified name after stdlib lookup
- **WHEN** атом содержит `QUALIFIER/MEMBER`
- **THEN** qualifier разрешается через require aliases текущего namespace либо используется как имя namespace
- **AND** member ищется в globals этого namespace

#### Scenario: Missing symbol
- **WHEN** символ не удаётся разрешить
- **THEN** возникает `Eval_error` с сообщением `symbol not found: NAME`

#### Scenario: Keywords остаются строками
- **WHEN** вычисляются `(= :name "name")` и `(:name {:name 42})`
- **THEN** результаты — true и 42

### Requirement: The evaluator SHALL support core control and binding forms

Evaluator SHALL поддерживать top-level `def`, `compiler/ns`, `deps`, `quote`, `if`, `fn*`, `let*` и `do`. Bindings `let*` и параметры `fn*` SHALL поддерживать символы, последовательную и ассоциативную деструктуризацию и их вложенные сочетания. Каждый top-level шаблон параметра SHALL потреблять один аргумент. Только nil и boolean false SHALL быть ложными. Quote SHALL сохранять тип литералов; прочие атомы SHALL становиться символами без lookup. Ассоциативная деструктуризация SHALL использовать то же равенство ключей, что `get`.

#### Scenario: Define in current namespace
- **WHEN** вычисляется top-level `(def name value)`
- **THEN** `value` вычисляется и сохраняется как `name` в globals текущего namespace

#### Scenario: Compiler namespace form
- **WHEN** вычисляется `(compiler/ns "app" (("dep.ns" "alias")) ())`
- **THEN** текущим namespace становится `app`
- **AND** alias `alias` разрешается в namespace `dep.ns`
- **AND** результатом формы является `nil`

#### Scenario: Загрузка зависимостей
- **WHEN** форма `deps` успешно загружает зависимости, включая пустой набор
- **THEN** результатом формы является `nil`
- **AND** имена пакетов и версии являются строками; `(deps {:package "version"})` сохраняет работоспособность

#### Scenario: Quote form
- **WHEN** вычисляется `(quote value)`
- **THEN** nil, boolean, строки и числа сохраняют тип, остальные атомы становятся символами, списки цитируются рекурсивно без вычисления содержимого
- **AND** ранее выполненное раскрытие макросов внутри quote не меняется этим требованием

#### Scenario: Цитирование вложенного nil
- **WHEN** вычисляется `(= (get '(nil "nil") 0) nil)`
- **THEN** результат равен `true`
- **WHEN** вычисляется `(= (get '(nil "nil") 1) nil)`
- **THEN** результат равен `false`

#### Scenario: If without else
- **WHEN** условие `(if condition then)` ложно
- **THEN** результатом является `nil`

#### Scenario: Truthiness
- **WHEN** значение — nil либо boolean false
- **THEN** оно ложно
- **AND** строки, числа, символы и остальные значения истинны

#### Scenario: Lexical function closure
- **WHEN** `fn*` захватывает локальные bindings и позже вызывается
- **THEN** тело вычисляется с захваченными bindings и namespace замыкания

#### Scenario: Let bindings
- **WHEN** `let*` содержит пары имя/значение
- **THEN** значения вычисляются по порядку с доступом к предшествующим bindings
- **AND** тело вычисляется со всеми созданными bindings

#### Scenario: Sequential destructuring let binding
- **WHEN** шаблон `(list a b)` связывается со списком из двух элементов
- **THEN** `a` и `b` получают соответствующие элементы

#### Scenario: Associative destructuring let binding
- **WHEN** шаблон `(hash-map "name" n "age" a)` связывается с hash map
- **THEN** `n` и `a` получают значения по ключам `"name"` и `"age"`

#### Scenario: Missing associative destructuring key
- **WHEN** ключ ассоциативного шаблона отсутствует в hash map
- **THEN** соответствующее имя получает `nil`

#### Scenario: Nested destructuring let binding
- **WHEN** шаблон binding содержит последовательный или ассоциативный вложенный шаблон
- **THEN** имена связываются рекурсивным применением вложенного шаблона к выбранному значению

#### Scenario: Do body
- **WHEN** `do` содержит несколько выражений
- **THEN** они вычисляются по порядку и возвращается значение последнего

#### Scenario: Sequential destructuring function parameters
- **WHEN** параметр функции имеет шаблон `(list a b)` и получает список
- **THEN** `a` и `b` получают соответствующие элементы

#### Scenario: Associative and nested function parameters
- **WHEN** параметр содержит `(hash-map "name" n "tags" (list first-tag))` и получает соответствующую hash map
- **THEN** `n` и `first-tag` получают выбранные значения

#### Scenario: Missing function parameter values
- **WHEN** последовательный шаблон выбирает отсутствующий индекс либо ассоциативный шаблон выбирает отсутствующий ключ
- **THEN** соответствующее имя получает `nil`

#### Scenario: Extra function argument collection values
- **WHEN** список-аргумент содержит больше элементов, чем последовательный шаблон параметра
- **THEN** лишние элементы игнорируются

#### Scenario: Цитирование boolean
- **WHEN** вычисляются `(= 'false false)`, `(= 'false "false")` и `(if (get '(false true) 0) 1 2)`
- **THEN** результаты — true, false и 2

#### Scenario: Выбранная ветка и управляющие макросы
- **WHEN** вычисляются `(if "false" 1 (assert false))`, `(and "false" 7)`, `(or false "false")` и `(if-let [x "false"] x "other")`
- **THEN** результаты — 1, 7, `"false"` и `"false"`; невыбранная ветка if не вычисляется

#### Scenario: Цитированные числа и символы
- **WHEN** вычисляются `(= '42 42)`, `(= '0.25 0.25)`, `(= 'foo 'foo)`, `(= 'foo "foo")` и `(= (get '(42 "42" foo) 1) 42)`
- **THEN** результаты — true, true, true, false и false

## ADDED Requirements

### Requirement: Eval SHALL отличать nil от текстового значения во всех операциях

Eval SHALL считать `nil` равным только `nil`, в том числе при структурном сравнении коллекций и поиске ключей hash map. Операции `not` и `assert` SHALL использовать ту же истинность, что и `if`. `get` и `get-in` SHALL распространять отсутствие данных как `nil`, а строку `"nil"` обрабатывать как обычный скаляр. Преобразование `nil` в текст через `str` SHALL давать `nil`, в том числе внутри коллекций.

#### Scenario: Равенство nil и текста
- **WHEN** вычисляются `(= nil nil)`, `(= nil "nil")` и `(not= nil "nil")`
- **THEN** результаты равны `true`, `false` и `true` соответственно

#### Scenario: Равенство коллекций
- **WHEN** вычисляются `(= [nil] [nil])`, `(= [nil] ["nil"])` и `(= {:x nil} {:x "nil"})`
- **THEN** результаты равны `true`, `false` и `false` соответственно

#### Scenario: Различные ключи hash map
- **WHEN** вычисляется `(let [m (hash-map nil 1 "nil" 2)] (str (get m nil) " " (get m "nil")))`
- **THEN** результат равен `"1 2"`

#### Scenario: Логические операции
- **WHEN** вычисляются `(if "nil" 1 2)`, `(not nil)`, `(not "nil")` и `(assert "nil")`
- **THEN** результаты равны `1`, `true`, `false` и `true` соответственно
- **WHEN** вычисляется `(assert nil)`
- **THEN** возникает `Eval_error` с сообщением `assertion failed`

#### Scenario: Отсутствующие данные
- **WHEN** вычисляются `(get {} :missing)`, `(get [] 0)`, `(get nil :x)` и `(get-in {:x nil} [:x :y])`
- **THEN** каждый результат равен `nil` и не равен строке `"nil"`

#### Scenario: Текст nil в позиции коллекции
- **WHEN** вычисляется `(get "nil" :x)` или `(get-in {:x "nil"} [:x :y])`
- **THEN** возникает ошибка eval, как при обходе другого строкового скаляра

#### Scenario: Текстовое представление
- **WHEN** вычисляются `(str nil)`, `(str [nil])` и `(str {"x" nil})`
- **THEN** результаты равны `"nil"`, `"(nil)"` и `"{x nil}"` соответственно
- **AND** результат `(str nil)` является истинным текстовым значением, отличным от `nil`
