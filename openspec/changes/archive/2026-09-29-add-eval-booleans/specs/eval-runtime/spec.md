# Spec Delta

## MODIFIED Requirements

### Requirement: The eval target SHALL return only the final symbol value

Runner SHALL разобрать и раскрыть формы, вычислить их через eval и вернуть текст последнего scalar value: текстового значения, boolean либо nil. Boolean SHALL выводиться как `true` или `false`, nil — как `nil`. При отсутствии результата или финальном значении коллекции, функции либо Atom runner SHALL вернуть пустую строку. Историческое имя требования не ограничивает перечисленные типы.

#### Scenario: Evaluate sample test function
- **WHEN** после тела fixture вычисляется `(test)`, возвращающий текстовое значение
- **THEN** runner возвращает его текст

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

### Requirement: The evaluator SHALL resolve atoms by the implemented lookup order

Evaluator SHALL сначала распознавать строковые атомы в двойных кавычках. Для остальных атомов SHALL проверяться locals, globals текущего namespace, stdlib, qualified references и литералы в этом порядке. Литералы `true`/`false`, не разрешённые через bindings, SHALL давать boolean, отличный от одноимённого текста. Nil SHALL оставаться отдельным значением отсутствия. Числовые литералы на этом этапе SHALL сохранять существующее текстовое представление.

#### Scenario: String atom
- **WHEN** вычисляется строковый атом `"abc"`, `"nil"` или `"false"`
- **THEN** результатом является текстовое значение с содержимым строки

#### Scenario: Literal atom
- **WHEN** атом `true`, `false` или числовой атом не разрешён через bindings
- **THEN** `true` и `false` дают boolean, а числовой атом — текстовое значение числа

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

### Requirement: The evaluator SHALL support core control and binding forms

Evaluator SHALL поддерживать top-level `def`, `compiler/ns`, `deps`, `quote`, `if`, `fn*`, `let*` и `do`. Bindings `let*` и параметры `fn*` SHALL поддерживать символы, последовательную и ассоциативную деструктуризацию и их вложенные сочетания. Каждый top-level шаблон параметра SHALL потреблять один аргумент. Истинность SHALL зависеть от значения, а не текстового представления: ложны только nil и boolean false. Quote SHALL сохранять nil и boolean, остальные атомы пока преобразуются в текстовые значения.

#### Scenario: Define in current namespace
- **WHEN** вычисляется top-level `(def name value)`
- **THEN** value вычисляется и сохраняется в globals текущего namespace

#### Scenario: Compiler namespace form
- **WHEN** вычисляется `(compiler/ns "app" (("dep.ns" "alias")) ())`
- **THEN** текущим namespace становится `app`, alias разрешается в `dep.ns`
- **AND** результат формы — nil

#### Scenario: Quote form
- **WHEN** вычисляется `(quote value)`
- **THEN** nil и boolean сохраняют тип, остальные атомы становятся текстовыми значениями, списки цитируются рекурсивно без вычисления содержимого
- **AND** строки `"nil"` и `"false"` остаются текстом

#### Scenario: If without else
- **WHEN** условие `(if condition then)` ложно
- **THEN** результат — nil

#### Scenario: Truthiness
- **WHEN** значение равно nil либо boolean false
- **THEN** оно ложно
- **AND** остальные значения, включая `"false"`, `"nil"`, `0`, `""` и пустые коллекции, истинны

#### Scenario: Lexical function closure
- **WHEN** `fn*` захватывает locals и позже вызывается
- **THEN** тело вычисляется с захваченными bindings и namespace замыкания

#### Scenario: Let bindings
- **WHEN** `let*` содержит пары имя/значение
- **THEN** значения вычисляются по порядку с доступом к предыдущим bindings, а тело — со всеми bindings

#### Scenario: Sequential destructuring let binding
- **WHEN** `(list a b)` связывается со списком из двух элементов
- **THEN** a и b получают соответствующие элементы

#### Scenario: Associative destructuring let binding
- **WHEN** `(hash-map "name" n "age" a)` связывается с hash map
- **THEN** n и a получают значения соответствующих ключей

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
- **WHEN** `deps` успешно загружает зависимости, включая пустой набор
- **THEN** результат — nil

#### Scenario: Цитирование вложенного nil
- **WHEN** вычисляются `(= (get '(nil "nil") 0) nil)` и `(= (get '(nil "nil") 1) nil)`
- **THEN** результаты — true и false

#### Scenario: Цитирование boolean
- **WHEN** вычисляются `(= 'false false)`, `(= 'false "false")` и `(if (get '(false true) 0) 1 2)`
- **THEN** результаты — true, false и 2

#### Scenario: Выбранная ветка и управляющие макросы
- **WHEN** вычисляются `(if "false" 1 (assert false))`, `(and "false" 7)`, `(or false "false")` и `(if-let [x "false"] x "other")`
- **THEN** результаты — 1, 7, `"false"` и `"false"`; невыбранная ветка if не вычисляется

## ADDED Requirements

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
