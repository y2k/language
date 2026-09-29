# Spec Delta

## ADDED Requirements

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
