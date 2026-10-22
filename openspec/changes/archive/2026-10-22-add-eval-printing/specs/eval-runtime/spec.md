# Spec Delta

## ADDED Requirements

### Requirement: Eval SHALL предоставлять println и eprintln для построчного вывода

Eval SHALL предоставлять без импорта обычные callable bindings `println` и `eprintln`. Каждая функция SHALL принимать ноль или больше значений любых runtime-типов, преобразовывать каждое по правилам `str`, соединять тексты одним пробелом и добавлять перевод строки. `println` SHALL писать в stdout, `eprintln` — в stderr. Вызов без аргументов SHALL печатать только перевод строки.

#### Scenario: Выбор потока
- **WHEN** eval выполняет `(println "hello")` либо `(eprintln "hello")`
- **THEN** функция пишет ровно `hello` и перевод строки соответственно в stdout либо stderr
- **AND** сама функция не пишет в другой поток

#### Scenario: Несколько аргументов и форматирование
- **WHEN** каждая функция получает аргументы `"a"`, `42`, `0.25`, `false`, `nil`, `:key`, `'sym`, `[1 nil]`, `{:x false}`
- **THEN** она пишет `a 42 0.25 false nil key sym (1 nil) {x false}` и перевод строки
- **WHEN** аргументы включают функцию, Atom и regex
- **THEN** используются существующие представления `#<function>`, `#<atom>` и `#<regex>`

#### Scenario: Пустой вызов и пустые строки
- **WHEN** вызывается `(println)` либо `(eprintln)`
- **THEN** в соответствующий поток записывается ровно один перевод строки
- **WHEN** вызывается соответствующая функция с `""` и `"x"`
- **THEN** записывается ` x` и перевод строки, без удаления начального пробела

#### Scenario: Функция как значение
- **WHEN** выполняется `(let [emit eprintln] (emit "hello"))` либо аналогичный вызов с println
- **THEN** локальное функциональное значение выполняет тот же вывод в соответствующий поток

### Requirement: Eval функции вывода SHALL возвращать nil

После успешной записи `println` и `eprintln` SHALL возвращать отдельное nil, не строку. Возвращаемое значение SHALL быть ложным и сохранять обычное равенство nil независимо от числа и типов напечатанных аргументов.

#### Scenario: Тип и истинность результата
- **WHEN** результат любой из функций сохраняется в binding r
- **THEN** `(= r nil)` возвращает true, `(= r "nil")` возвращает false, `(if r true false)` возвращает false

## MODIFIED Requirements

### Requirement: The eval target SHALL return only the final symbol value

Runner SHALL вычислять формы через eval и возвращать только финальный scalar value: строку, символ, число, boolean либо nil. Строки и символы SHALL возвращаться без кавычек, числа — по правилам `str`, boolean — как `true`/`false`, nil — как `nil`. Финальная коллекция, функция, Atom либо отсутствие результата SHALL давать пустую строку. Побочный вывод println/eprintln SHALL выполняться отдельно и не входить в строку результата runner. CLI SHALL печатать результат runner в stdout с переводом строки. Историческое имя требования не ограничивает перечисленные типы.

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

#### Scenario: Диагностика и финальный результат CLI
- **WHEN** CLI с target eval исполняет только `(eprintln "hello")`
- **THEN** stderr равен `"hello\n"`, stdout равен `"nil\n"`, код завершения равен 0
- **WHEN** CLI исполняет только `(println "hello")`
- **THEN** stdout равен `"hello\nnil\n"`, stderr пуст, код завершения равен 0

#### Scenario: Вывод не заменяет полезный результат
- **WHEN** CLI с target eval исполняет `(do (println "out") (eprintln "err") "done")`
- **THEN** stdout равен `"out\ndone\n"`, stderr равен `"err\n"`, код завершения равен 0
- **AND** runner возвращает строку `done`, не объединённый журнал вывода
