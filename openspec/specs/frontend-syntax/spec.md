# Frontend Syntax Spec

## Purpose

Describe the current parser, AST metadata, macro expansion, and namespace macro behavior.

## Requirements

### Requirement: The parser SHALL produce s-expressions with source metadata

The parser SHALL read source text into atoms and lists while preserving line/column metadata on parsed nodes.

#### Scenario: Parse multiple top-level forms
- **WHEN** the input contains multiple valid forms separated by whitespace
- **THEN** parsing returns all top-level forms in source order

#### Scenario: Track locations
- **WHEN** a parsed atom or list begins at a source offset
- **THEN** its metadata records the corresponding 1-based line and column

### Requirement: The parser SHALL ignore whitespace and line comments

Whitespace and `;` line comments SHALL be skipped between forms.

#### Scenario: Ignore semicolon comments
- **WHEN** a line contains a `;` comment
- **THEN** characters through the next newline are ignored by the parser

### Requirement: The parser SHALL support atom, string, quote, and bracket forms

Парсер SHALL поддерживать atoms, строковые atoms в двойных кавычках, quote shorthand и списки с разделителями `()`, `[]`, `{}`. Внутри исходной строки пары `\"`, `\\`, `\n`, `\t`, `\r` SHALL декодироваться соответственно в кавычку, один обратный слеш, LF, TAB и CR до исполнения или целевой компиляции. Декодирование SHALL выполняться однократно: полученные символы SHALL NOT повторно интерпретироваться как исходные escapes. Остальные пары обратного слеша и следующего символа SHALL сохраняться буквально; в частности, `\q`, `\b`, `\f`, `\/` и начало `\u0041` не получают специального значения.

#### Scenario: Quote shorthand
- **WHEN** вход содержит `'x`
- **THEN** парсер возвращает `(quote x)`

#### Scenario: Bracket-specific lists
- **WHEN** вход содержит `(a)`, `[a]` или `{a}`
- **THEN** AST различает круглые, квадратные и фигурные скобки

#### Scenario: Decode newline escape
- **WHEN** вход содержит исходную строку `"a\nb"`, где `\n` состоит из обратного слеша и `n`
- **THEN** значение содержит ровно один LF между `a` и `b`, а не обратный слеш и `n`

#### Scenario: Preserve newline value across targets
- **WHEN** строка с декодированным LF исполняется в eval или компилируется для JavaScript либо Java
- **THEN** каждый target наблюдает runtime-строку с одним LF в этой позиции

#### Scenario: Декодировать кавычку и обратный слеш
- **WHEN** вход содержит строки `"a\"b"` и `"a\\b"`
- **THEN** их значения содержат между `a` и `b` соответственно одну кавычку и один обратный слеш

#### Scenario: Декодировать табуляцию и возврат каретки
- **WHEN** вход содержит строки `"a\tb"` и `"a\rb"`
- **THEN** их значения содержат между `a` и `b` соответственно один TAB и один CR

#### Scenario: Не декодировать результат повторно
- **WHEN** вход содержит строку `"\\n"`
- **THEN** значение состоит ровно из обратного слеша и буквы `n`, без LF

#### Scenario: Сохранить неизвестные escape-пары
- **WHEN** вход содержит строки `"\q"`, `"\b"`, `"\f"`, `"\/"` и `"\u0041"`
- **THEN** каждая строка сохраняет все символы между внешними кавычками, включая обратный слеш, без ошибки неизвестного escape

#### Scenario: Сохранить значения во всех targets
- **WHEN** строка с любыми поддержанными или сохранёнными неизвестными escape-парами исполняется в eval либо компилируется для JavaScript или Java
- **THEN** все targets наблюдают одинаковую последовательность символов, установленную frontend

#### Scenario: Сохранить HTML из issue #13
- **WHEN** вход содержит `(def value "<div data-post=\"serbia/4\"></div>")`
- **THEN** runtime-значение `value` равно `<div data-post="serbia/4"></div>` без обратных слешей перед кавычками

### Requirement: The parser SHALL attach type annotation metadata

The parser SHALL attach `^TYPE` metadata to the immediately following atom or list.

#### Scenario: Annotate a function expression
- **WHEN** the input contains `^java.util.function.Function (fn [x] x)`
- **THEN** the function list metadata contains the annotation string

### Requirement: Built-in macros SHALL desugar user syntax into core forms

Macro expansion SHALL transform recognized syntactic forms before backend execution or compilation. После каждого раскрытия outer form SHALL снова проверяться built-in macros до тех пор, пока она не перестанет быть распознаваемой macro form; metadata результирующей формы SHALL сохраняться в последующих раскрытиях. Binding forms in `let` SHALL preserve pattern structure after normal collection and keyword expansion so backends can distinguish symbol, sequential, and associative binding patterns. Brace binding patterns in `let` and `fn` SHALL recursively normalize each reverse pair `binding :keyword` to the existing key-first form `:keyword binding` before collection and keyword expansion. Эта нормализация SHALL не применяться к обычным map-выражениям, прямым core-формам `let*`/`fn*` или явно записанным `(hash-map ...)` patterns. `(def- name value)` SHALL become core `(def name value)` with private metadata. `(defn- name params body...)` SHALL become private `(defn name params body...)` and complete the normal `defn` and `fn` expansions into a private core `def` containing `fn*`. Двухаргументная форма `(:key collection)` SHALL раскрывать keyword-вызов в `(get collection "key")` до обычного преобразования keyword в строковый литерал.

#### Scenario: Desugar collection literals
- **WHEN** the input contains `[1 2]` or `{:a 1}`
- **THEN** vectors become `(list 1 2)` and maps become `(hash-map "a" 1)` after keyword expansion

#### Scenario: Desugar keyword lookup
- **WHEN** the input contains `(:TELEGRAM_WEBHOOK_SECRET env)`
- **THEN** it becomes `(get env "TELEGRAM_WEBHOOK_SECRET")`

#### Scenario: Desugar binding and function forms
- **WHEN** the input contains `let`, `fn`, or `defn`
- **THEN** they become `let*`, `fn*`, or a non-private `def` plus `fn*`

#### Scenario: Desugar private value definition
- **WHEN** the input contains `(def- storage value)`
- **THEN** it becomes a core `def` of `storage` with `value` and private metadata

#### Scenario: Desugar private function definition through public function macro
- **WHEN** the input contains `(defn- helper [x] x)`
- **THEN** it first becomes a private `defn`, then a private core `def` containing `fn*`

#### Scenario: Preserve private metadata through chained macro expansion
- **WHEN** a macro expansion produces another recognized macro form with private metadata
- **THEN** the later expansion receives and preserves that private metadata

#### Scenario: Preserve sequential let binding patterns
- **WHEN** the input contains `(let [[a b] xs] body)`
- **THEN** macro expansion preserves the binding pattern as a sequential pattern in the resulting `let*` bindings

#### Scenario: Preserve associative let binding patterns
- **WHEN** the input contains `(let [{:name n :age a} user] body)`
- **THEN** macro expansion preserves the binding pattern as an associative pattern in the resulting `let*` bindings

#### Scenario: Normalize reversed associative let bindings
- **WHEN** the input contains `(let [{url :url props :props} value] body)`
- **THEN** macro expansion produces canonical bindings `(hash-map "url" url "props" props)` in the resulting `let*`

#### Scenario: Normalize reversed associative function parameters
- **WHEN** a `fn`, `defn`, or `defn-` parameter contains `{url :url}`
- **THEN** macro expansion produces a canonical `(hash-map "url" url)` parameter pattern in the resulting `fn*`

#### Scenario: Normalize nested reversed associative bindings
- **WHEN** a `let` binding or `fn` parameter contains reverse keyword pairs nested in sequential or associative brace patterns
- **THEN** every nested reverse keyword pair is normalized to key-first order

#### Scenario: Leave map expressions outside binding patterns unchanged
- **WHEN** `{url :url}` occurs as a value expression rather than a `let` binding pattern or `fn` parameter
- **THEN** macro expansion preserves its pair order and produces `(hash-map url "url")`

#### Scenario: Preserve short-circuit behavior in logical macros
- **WHEN** the input contains `and` or `or`
- **THEN** expansion uses generated temporaries, `let*`, and `if` so operands are evaluated at most once and only as needed

#### Scenario: Desugar threading macros
- **WHEN** the input contains `->` or `->>`
- **THEN** forms are rewritten by inserting the threaded value as first or last argument respectively

#### Scenario: Desugar interop shorthand
- **WHEN** the input contains `(.method obj args...)` or `(Class. args...)`
- **THEN** it becomes `(. obj method args...)` or `(new Class args...)`

### Requirement: Макрос cond SHALL выбирать первую truthy-ветвь

Встроенный макрос `(cond test result ...)` SHALL раскрывать последовательный выбор в core-формы с семантикой существующего `if` на targets `eval`, `js` и `java`. Достигнутые условия SHALL вычисляться в исходном порядке не более одного раза каждое. Макрос SHALL возвращать результат первой ветви с truthy-условием, включая результат `false` или `nil`. Результаты несовпавших ветвей и все условия и результаты после совпадения SHALL NOT вычисляться. Если совпадений нет, включая пустой `(cond)`, результат SHALL быть `nil`. `:else` SHALL обрабатываться как обычное truthy-условие на своём месте, без перестановки ветвей.

#### Scenario: Пустой список ветвей
- **WHEN** исполняется `(cond)`
- **THEN** результат равен `nil`

#### Scenario: Отсутствие совпадений
- **WHEN** исполняется `(cond false 1 nil 2)`
- **THEN** результат равен `nil`

#### Scenario: Первое совпадение
- **WHEN** исполняется `(cond false 1 true 2 :else 3)`
- **THEN** результат равен `2`

#### Scenario: Falsey-результат выбранной ветви
- **WHEN** исполняются `(cond true false :else 3)` и `(cond true nil :else 3)`
- **THEN** результаты равны соответственно `false` и `nil`, без перехода к следующей ветви

#### Scenario: Обычное условие else
- **WHEN** исполняется `(cond :else 1 true 2)`
- **THEN** результат равен `1`

#### Scenario: Существующая truthiness
- **WHEN** исполняется `(cond 0 1 :else 2)`
- **THEN** результат равен `1`, как у соответствующего `if`

#### Scenario: Использование в позиции значения
- **WHEN** исполняется `(+ 10 (cond false 1 :else 2))`
- **THEN** результат равен `12` на каждом из трёх targets

#### Scenario: Порядок и однократность условий
- **WHEN** первое условие записывает `test-a` в журнал и возвращает `false`, второе записывает `test-b` и возвращает `true`, а его результат записывает `result-b`
- **THEN** журнал содержит ровно последовательность `test-a,test-b,result-b`
- **AND** побочные эффекты результата первой ветви и всех условий и результатов после второй ветви отсутствуют

### Requirement: Макрос cond SHALL отклонять неполные пары при раскрытии

Нечётное число форм после `cond` SHALL вызывать ошибку macro expansion с сообщением, указывающим на `cond` и необходимость пар условия и результата. Проверка SHALL охватывать всю форму независимо от достижимости ветвей во время исполнения.

#### Scenario: Условие без результата
- **WHEN** frontend раскрывает `(cond true)`
- **THEN** раскрытие завершается ошибкой неполной пары

#### Scenario: Неполная пара после истинного условия
- **WHEN** frontend раскрывает `(cond true 1 false)` или `(cond :else 1 true)`
- **THEN** раскрытие завершается той же ошибкой до исполнения программы

### Requirement: Раскрытие cond SHALL сохранять исходные metadata

Раскрытие `cond` SHALL сохранять metadata исходных условий и результатов. Созданные формы управления и служебные atoms SHALL наследовать metadata исходного вызова `cond`, включая его позицию в исходнике.

#### Scenario: Позиции исходных и созданных форм
- **WHEN** frontend раскрывает многострочный `cond`, условия и результаты которого имеют разные позиции
- **THEN** исходные условия и результаты сохраняют свои позиции
- **AND** созданные формы управления и служебные atoms имеют metadata вызова `cond`

### Requirement: Макрос `case` SHALL раскрывать выбор по значению в core forms

Встроенный макрос `(case value match result ... fallback?)` SHALL вычислять `value` ровно один раз, последовательно сравнивать его с `match` через `=` и раскрывать выбор в `let*` и вложенные `if`. Макрос SHALL вычислять только `result` первой совпавшей ветви. Последняя форма без парного `result` SHALL служить fallback; если fallback отсутствует и совпадений нет, результат SHALL быть `nil`.

#### Scenario: Выбрать совпавшую ветвь
- **WHEN** вход содержит `(case 2 1 "one" 2 "two" "other")`
- **THEN** раскрытая форма сравнивает значение с вариантами по порядку и возвращает `"two"`

#### Scenario: Использовать fallback
- **WHEN** ни один `match` не равен вычисленному значению и после пар присутствует fallback
- **THEN** раскрытая форма возвращает fallback

#### Scenario: Вернуть nil без fallback
- **WHEN** ни один `match` не равен вычисленному значению и fallback отсутствует
- **THEN** раскрытая форма возвращает `nil`

#### Scenario: Вычислить проверяемое выражение один раз
- **WHEN** `value` содержит вызов с наблюдаемым побочным эффектом
- **THEN** раскрытая форма связывает результат вызова с fresh именем через `let*` и выполняет вызов ровно один раз

#### Scenario: Не вычислять невыбранные результаты
- **WHEN** совпадение найдено до конца списка ветвей
- **THEN** результаты последующих ветвей и fallback не вычисляются

### Requirement: Макрос `if-let` SHALL последовательно проверять символьные bindings

Встроенный макрос `(if-let [name expression ...] then else?)` SHALL принимать непустой bracket-вектор из пар `name` и `expression`, где каждый `name` является symbol, и одну обязательную форму `then` с не более чем одной формой `else`. Макрос SHALL вычислять binding expressions слева направо не более одного раза. Каждый следующий `expression` SHALL видеть предыдущие bindings. При первом значении, ложном по существующей семантике `if`, макрос SHALL прекратить вычисление оставшихся expressions и вычислить `else`; если `else` отсутствует, результат SHALL быть `nil`. `then` SHALL вычисляться только после truthy-результата всех bindings и SHALL видеть их все. Доступность bindings в `else` не входит в контракт, и программа SHALL NOT полагаться на неё.

#### Scenario: Вычислить зависимые bindings последовательно
- **WHEN** вход содержит `(if-let [user (find-user) id (get user "id")] id "missing")` и оба binding expressions возвращают truthy-значения
- **THEN** `(find-user)` вычисляется один раз до `(get user "id")`, второй expression видит `user`, а результатом становится `id`

#### Scenario: Остановиться на первом falsey binding
- **WHEN** binding expression возвращает `false` или `nil`
- **THEN** последующие binding expressions и `then` не вычисляются, а вычисляется `else`

#### Scenario: Вернуть nil без else
- **WHEN** форма `(if-let [value expression] then)` получает falsey `value`
- **THEN** результатом является `nil`

#### Scenario: Не вычислять else после успешных bindings
- **WHEN** все binding expressions возвращают truthy-значения
- **THEN** вычисляется только `then`, а `else` не вычисляется

#### Scenario: Не предоставлять bindings ветке else
- **WHEN** выполнение переходит в `else`
- **THEN** программа не получает гарантии доступности ни одного имени, объявленного в binding-векторе `if-let`

#### Scenario: Отклонить несимвольное имя
- **WHEN** binding name является collection, string, number, keyword, `nil`, `true` или `false`
- **THEN** macro expansion отклоняет форму

#### Scenario: Отклонить malformed форму
- **WHEN** binding collection не является bracket-вектором, пуста, содержит нечётное число элементов либо после неё отсутствует `then` или присутствует более одной формы `else`
- **THEN** macro expansion отклоняет форму

### Requirement: Макросы SHALL дешугорировать аннотации типов символьных параметров

При дешугорировании `fn` и производных от него `defn` или `defn-` аннотация `^TYPE` на символьном параметре SHALL создавать fresh формальный параметр и локальный `let*` binding исходного имени к `(cast TYPE FRESH)`. Макрос SHALL сохранять порядок параметров, арность функции и тело после initial bindings. Это требование применяется только к символьным параметрам.

#### Scenario: Аннотированный параметр fn
- **WHEN** вход содержит `(fn [^java.util.List xs] (.size xs))`
- **THEN** результат содержит `fn*` с одним fresh символьным параметром и local binding `xs` к `(cast java.util.List FRESH)` до вызова `.size`

#### Scenario: Несколько аннотированных параметров
- **WHEN** вход содержит функцию с двумя символьными параметрами, аннотированными разными типами
- **THEN** результат сохраняет два формальных параметра и создаёт local cast binding для каждого исходного имени в исходном порядке

#### Scenario: Аннотированный параметр defn
- **WHEN** вход содержит `(defn size [^java.util.List xs] (.size xs))`
- **THEN** функция, созданная после дешугорирования `defn`, содержит local cast binding для `xs`

### Requirement: The namespace macro SHALL lower namespace declarations

The `ns` macro SHALL lower namespace declarations into `compiler/ns` with string namespace, require pairs, and import pairs. An `:import` clause SHALL accept one or more bracket import vectors; each vector SHALL contain a package symbol followed by zero or more class symbols. The macro SHALL emit import pairs in source order across vectors and classes.

#### Scenario: Lower require aliases
- **WHEN** the input contains `(ns app.main (:require [io.math.core :as mc]))`
- **THEN** it becomes a `compiler/ns` form with the namespace `io.math.core` paired to alias `mc`

#### Scenario: Lower import classes
- **WHEN** the input contains `(ns app.main (:import [java.time LocalDate]))`
- **THEN** it records `LocalDate` mapped to `java.time.LocalDate`

#### Scenario: Lower multiple import vectors
- **WHEN** the input contains `(ns app.main (:import [java.time LocalDate] [java.util UUID]))`
- **THEN** it records `LocalDate` mapped to `java.time.LocalDate` followed by `UUID` mapped to `java.util.UUID`
