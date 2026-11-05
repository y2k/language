# Spec Delta

## ADDED Requirements

### Requirement: CLI SHALL предоставлять текстовый target fex

CLI SHALL принимать `--target fex`, читать исходник из stdin и возвращать один текстовый bundle в stdout без записи code-файлов и без требования output directory. Bundle SHALL содержать RHS верхнеуровневых `def` в исходном порядке. `--source-map` SHALL оставаться доступным только для JS. Существующие eval/JS/Java SHALL сохранять свои интерфейсы.

#### Scenario: Несколько определений
- **WHEN** исходник namespace `app.logic` содержит `def a` и `def b` и компилируется в `fex`
- **THEN** stdout содержит пары для `app.logic.a` и `app.logic.b` в указанном порядке
- **AND** каждый payload кодирует только соответствующий RHS

#### Scenario: Каталог не указан
- **WHEN** CLI получает `--target fex` без `--output-dir`
- **THEN** корректный source компилируется в текстовый stdout без создания code-файлов

### Requirement: Bundle SHALL содержать пары имени и Base64 RHS

На каждое определение bundle SHALL содержать ровно две строки: canonical key и стандартный Base64 с padding полного текста RHS, включая переводы строк. Compiler SHALL завершать обе строки LF. Заголовок, trailer, счётчик и дополнительные пустые строки SHALL отсутствовать. Внешнее кодирование SHALL NOT менять формат FEX payload.

#### Scenario: Точная упаковка RHS
- **WHEN** компилируется `(ns app.logic) (def answer 42)`
- **THEN** bundle состоит из `app.logic.answer` и `aW50IDQyCg==`, каждая строка заканчивается LF
- **AND** декодированный payload равен bytes `int 42` с завершающим LF

### Requirement: FEX compiler SHALL использовать общий frontend

FEX compiler SHALL использовать существующее раскрытие макросов и поддерживать literals, `quote`, `if`, `do`, `let`, `fn`, вызовы, коллекции и деструктуризацию в поддерживаемом переносимом подмножестве. Пользовательские locals SHALL сохранять имена; необходимые временные имена SHALL создаваться через `Gensym`. Host interop и верхнеуровневые формы кроме namespace и `def` SHALL отклоняться.

#### Scenario: Макросы и деструктуризация
- **WHEN** программа использует `defn`, `cond`, векторную и map-деструктуризацию в `let` и параметрах
- **THEN** файлы содержат раскрытые core-формы и именованные bindings для деструктуризации, без исходных макросов

#### Scenario: Цитированные значения
- **WHEN** компилируется `quote` атомов и вложенных списков
- **THEN** атомы представлены строковыми данными, как в существующих compiled targets
- **AND** `'nil` даёт строку `nil`, а не ложный литерал

#### Scenario: Цитирование после desugar
- **WHEN** компилируется `'[1]`
- **THEN** результат представлен вызовом `list` со строками `list` и `1`, как на JS/Java после раскрытия vector syntax
- **AND** compiler цитирует результат существующего desugar, не меняя frontend на Clojure quote

#### Scenario: Непереносимая форма
- **WHEN** RHS содержит `raw-code` или непосредственный вызов host interop
- **THEN** компиляция завершается ошибкой, а не выдаёт неисполняемый файл

#### Scenario: Host-формы после раскрытия
- **WHEN** source использует `.method`, `Class.`, `cast`, типизированный параметр, раскрывающийся в `cast`, `instance?`, `gen-class`, `export-default`, `:import` или строковый host `:require`
- **THEN** FEX compilation отклоняет соответствующую форму без добавления host semantics

#### Scenario: Недопустимый верхний уровень
- **WHEN** source после desugar содержит верхнеуровневый вызов или литерал вместо namespace или `def`
- **THEN** FEX compilation завершается ошибкой

### Requirement: FEX compiler SHALL разрешать именованные ссылки

Compiler SHALL учитывать lexical shadowing, namespace, aliases и собственные определения при выборе `local`, `global` и `builtin`. Глобальный ключ SHALL совпадать с именем файла. Namespace по умолчанию SHALL быть `user`. Требования namespace SHALL разрешать имена, но SHALL не запускать автоматическую загрузку зависимостей.

#### Scenario: Alias и затенение builtin
- **WHEN** функция обращается к `dep/value` через alias, а локальный binding или собственный `def` затеняет builtin
- **THEN** alias разрешается в полное глобальное имя
- **AND** затенённое имя записывается как `local` либо `global`, а не `builtin`

#### Scenario: Рекурсия и ссылки вперёд
- **WHEN** тело функции ссылается на собственное или более позднее определение того же source, в том числе затеняющее builtin
- **THEN** ссылка представлена `global` с canonical key этого определения
- **AND** compiler не загружает и не вычисляет определение

### Requirement: Имена FEX файлов SHALL быть однозначными и безопасными

Canonical key SHALL состоять из компонентов namespace и имени `def`, разделённых точками. В компонентах `[a-z0-9_-]` SHALL сохраняться, ASCII `A–Z` SHALL кодироваться как `!a–!z`, остальные UTF-8 bytes — `%HH` с заглавными hex digits. Один key SHALL использоваться в bundle, filename и `global` без повторного кодирования.

#### Scenario: Точка в имени определения
- **WHEN** компилируются `a.b/c` и `a/b.c`
- **THEN** получаются разные имена `a.b.c` и `a.b%2Ec`
- **AND** global references используют те же ключи без дополнительного percent escaping полей

#### Scenario: Коллизия регистра
- **WHEN** компилируются `app/foo`, `app/Foo` и `app/FOO`
- **THEN** keys равны `app.foo`, `app.!foo` и `app.!f!o!o`
- **AND** keys не совпадают даже при регистронезависимом сравнении

#### Scenario: Буквальный маркер и регистр namespace
- **WHEN** компилируются `App/!foo` и `app/Foo`
- **THEN** keys равны `!app.%21foo` и `app.!foo` и не конфликтуют

#### Scenario: Unicode в имени
- **WHEN** компилируются `app/Я` и `app/я`
- **THEN** keys равны `app.%D0%AF` и `app.%D1%8F`
- **AND** различие сохраняется без Unicode case folding файловой системы

### Requirement: Файл определения SHALL содержать плоские выражения

Декодированный compiler payload и распакованный файл SHALL содержать UTF-8 без BOM: одну запись на строку с разделителем LF и одним ASCII space между полями. Файл SHALL содержать только RHS одного `def`, с корнем в строке 1 и forward references по 1-based номерам строк внутри RHS. Имена переменных SHALL сохраняться; заголовок, обёртка `def`, пустые записи и инструкции возврата SHALL отсутствовать.

#### Scenario: Функция в отдельном файле
- **WHEN** компилируется `(ns app.logic) (defn calculate [x] (+ x 1))`
- **THEN** декодированный payload ключа `app.logic.calculate` содержит записи `fn 2 x`, `call 3 4 5`, `builtin +`, `local x`, `int 1` в этом порядке

### Requirement: Строковые значения SHALL использовать UTF-8 Base64

Compiler SHALL кодировать строки как `string base64` со стандартным Base64 с padding без переносов; пустую строку — как `string -`. Имена и числа SHALL оставаться текстовыми токенами. Percent escapes SHALL применяться к filename components, но не к строковым значениям.

#### Scenario: Точная сериализация строк
- **WHEN** компилируются пустая строка, Unicode, пробелы, TAB, LF, CR, кавычки и буквальный `%0A`
- **THEN** каждый payload соответствует стандартному Base64 исходных UTF-8 bytes, а пустая строка записана как `string -`
- **AND** строка `hello world` записана как `string aGVsbG8gd29ybGQ=`

### Requirement: FEX compiler SHALL сохранять структуру выражений

Compiler SHALL использовать записи `nil`, `true`, `false`, `int`, `float`, `string`, `local`, `global`, `builtin`, `fn`, `call`, `if`, `let`, `do`. `fn` SHALL содержать адрес тела и параметры; `call` — callee и аргументы в исходном порядке; `if` — условие и обе ветви; `let` — имя, значение и тело; `do` — последовательность адресов. Все адреса SHALL указывать на существующие последующие записи.

#### Scenario: Вложенные выражения
- **WHEN** компилируются функция с шестью параметрами, вложенные `let`, `if`, `do` и вызовы
- **THEN** записи сохраняют параметры, lexical shadowing, порядок аргументов и обе ветви `if`, не вычисляя исходные выражения
- **AND** пустой `do` представлен записью `do` без адресов

#### Scenario: Неявная ветвь и последовательное тело
- **WHEN** source содержит `if` без else, несколько bindings в `let` и несколько выражений тела функции
- **THEN** отсутствующая ветвь представлена `nil`, bindings вложены последовательно, а тело функции представлено `do` в исходном порядке
