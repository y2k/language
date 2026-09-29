# Spec Delta

## RENAMED Requirements

- FROM: `### Requirement: The eval target SHALL return only the final symbol value`
- TO: `### Requirement: The eval target SHALL return the final symbol or nil value`

## MODIFIED Requirements

### Requirement: The eval target SHALL return the final symbol or nil value

Runner SHALL разобрать исходный текст, раскрыть макросы, вычислить все формы через eval и вернуть текст последнего symbol value либо `nil`, если последнее значение равно `nil`. При отсутствии результата или другом типе последнего значения runner SHALL вернуть пустую строку.

#### Scenario: Evaluate sample test function
- **WHEN** после тела fixture вычисляется `(test)`, возвращающий symbol value
- **THEN** результатом runner является текст этого symbol value

#### Scenario: Финальное nil
- **WHEN** последним выражением является `nil`, `'nil` или `(if false 1)`
- **THEN** runner возвращает строку `nil`

#### Scenario: Non-symbol final result
- **WHEN** последним результатом является список, hash map, closure, atom либо результата нет
- **THEN** runner возвращает пустую строку

#### Scenario: Eval error result
- **WHEN** вычисление вызывает `Eval_error` с сообщением `MESSAGE`
- **THEN** runner возвращает `MESSAGE` как строку ошибки

### Requirement: The evaluator SHALL resolve atoms by the implemented lookup order

Evaluator SHALL сначала распознавать строковые атомы в двойных кавычках. Для остальных атомов SHALL проверяться локальные bindings, globals текущего namespace, stdlib, qualified references и литералы в указанном порядке. Неразрешённый через bindings литерал `nil` SHALL давать отдельное значение отсутствия, отличное от symbol value с текстом `nil`.

#### Scenario: String atom
- **WHEN** вычисляется строковый атом `"abc"` или `"nil"`
- **THEN** получается symbol value с текстом `abc` или `nil` соответственно

#### Scenario: Литерал nil
- **WHEN** атом `nil` не разрешён предшествующими этапами поиска
- **THEN** получается отдельное значение `nil`, не равное строке `"nil"`

#### Scenario: Literal atom
- **WHEN** атом равен `true`, `false` или разбирается как число с плавающей точкой и не разрешён через bindings
- **THEN** получается symbol value с исходным текстом атома

#### Scenario: Qualified name after stdlib lookup
- **WHEN** атом содержит `QUALIFIER/MEMBER`
- **THEN** qualifier разрешается через require aliases текущего namespace либо используется как имя namespace
- **AND** member ищется в globals этого namespace

#### Scenario: Missing symbol
- **WHEN** символ не удаётся разрешить
- **THEN** возникает `Eval_error` с сообщением `symbol not found: NAME`

### Requirement: The evaluator SHALL support core control and binding forms

Evaluator SHALL поддерживать top-level `def`, `compiler/ns`, `deps`, `quote`, `if`, `fn*`, `let*` и `do`. Bindings `let*` и параметры `fn*` SHALL поддерживать символы, последовательную и ассоциативную деструктуризацию, включая вложенные комбинации. Каждый top-level шаблон параметра SHALL потреблять один аргумент функции. Возвращаемое этими формами отсутствие значения SHALL быть отдельным `nil`, а не строкой `"nil"`.

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

#### Scenario: Quote form
- **WHEN** вычисляется `(quote value)`
- **THEN** атом `nil` становится отдельным значением `nil`, остальные атомы становятся symbol values, а списки рекурсивно преобразуются в runtime-списки без вычисления содержимого
- **AND** цитирование строки `"nil"` сохраняет её текстовое значение

#### Scenario: Цитирование вложенного nil
- **WHEN** вычисляется `(= (get '(nil "nil") 0) nil)`
- **THEN** результат равен `true`
- **WHEN** вычисляется `(= (get '(nil "nil") 1) nil)`
- **THEN** результат равен `false`

#### Scenario: If without else
- **WHEN** условие `(if condition then)` ложно
- **THEN** результатом является `nil`

#### Scenario: Truthiness
- **WHEN** значение равно `nil` или symbol value с текстом `false`
- **THEN** оно ложно
- **AND** все остальные значения, включая строку `"nil"`, истинны

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
