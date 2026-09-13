## ADDED Requirements

### Requirement: Java target SHALL предоставлять статическую проверку instance?

Java target SHALL поддерживать специальную форму `(instance? TYPE value)` с ровно двумя аргументами. `TYPE` SHALL быть статическим символом класса или интерфейса: коротким именем, разрешаемым обычными Java imports и правилами Java, либо полным именем. Операнд типа SHALL NOT вычисляться как значение языка. Generated Java SHALL использовать нативный `instanceof` без `Class/forName` и отражательной проверки типа. Результат SHALL быть boolean: `true` для экземпляра указанного класса, его подкласса или реализации указанного интерфейса, `false` для `nil` и несовместимого объекта. Числовые и boolean-значения SHALL проверяться как соответствующие boxed Java objects. Динамические Class-значения, параметризованные типы и отдельный синтаксис типов массивов не входят в контракт.

#### Scenario: Импортированный класс
- **WHEN** namespace содержит `(:import [java.util ArrayList])` и функция возвращает `(instance? ArrayList (ArrayList.))`
- **THEN** generated Java компилируется, содержит `instanceof` и функция возвращает `true`
- **AND** проверка не использует `Class/forName`

#### Scenario: Полное имя и подкласс
- **WHEN** функция возвращает `(instance? java.util.AbstractList (java.util.ArrayList.))`
- **THEN** результат равен `true`

#### Scenario: Интерфейс
- **WHEN** функция возвращает `(instance? java.util.List (java.util.ArrayList.))`
- **THEN** результат равен `true`

#### Scenario: Nil и несовместимый объект
- **WHEN** функции проверяют `(instance? String nil)` и `(instance? Integer "hello")`
- **THEN** generated Java компилируется и обе проверки возвращают `false`

#### Scenario: Boxing скаляров
- **WHEN** функции проверяют `(instance? Integer 42)`, `(instance? Boolean true)` и `(instance? String 42)`
- **THEN** результаты равны соответственно `true`, `true` и `false`

### Requirement: Java instance? SHALL сохранять однократное вычисление значения

При достижении формы `instance?` Java target SHALL вычислять `value` ровно один раз. Форма SHALL работать в условии, возвращаемом значении, аргументе вызова, binding и позиции с отброшенным результатом, включая `value` с `if`, `let` или `do`. Невыбранная ветвь SHALL NOT выполнять содержащуюся в ней проверку или её `value`.

#### Scenario: Побочный эффект значения
- **WHEN** `value` увеличивает счётчик и возвращает объект, проверяемый через `instance?`
- **THEN** счётчик увеличивается ровно на один и результат соответствует типу объекта

#### Scenario: Управляющая форма в значении
- **WHEN** `value` использует `let`, `do` и `if` для выбора объекта с наблюдаемым побочным эффектом
- **THEN** generated Java компилируется и выполняет эффекты выбранного пути ровно один раз

#### Scenario: Отброшенный результат
- **WHEN** функция выполняет `(instance? Object (make-value))` перед последним выражением тела
- **THEN** generated Java компилируется, `make-value` вызывается ровно один раз и функция возвращает последнее выражение

#### Scenario: Проверка в невыбранной ветви
- **WHEN** `instance?` находится в невыбранной ветви `if`
- **THEN** побочные эффекты её `value` отсутствуют

### Requirement: Java instance? SHALL диагностировать недопустимую форму и тип

Неверная арность, несимвольный операнд типа, literal вместо имени типа и primitive type, включая `void`, SHALL приводить к ошибке до запуска `javac`. Ошибка SHALL указывать `instance?`, причину и исходную позицию. Выражение в позиции типа SHALL NOT становиться допустимым из-за упрощения при компиляции. Неизвестное или недоступное имя класса SHALL отклоняться при компиляции generated Java через `javac`; Java-генератор не обязан проверять classpath. Точный текст ошибки `javac` не фиксируется.

#### Scenario: Неверная арность
- **WHEN** компилируются `(instance?)`, `(instance? String)` или `(instance? String value extra)`
- **THEN** генерация завершается ошибкой арности `instance?` с координатами формы

#### Scenario: Недопустимый операнд типа
- **WHEN** первым аргументом переданы `"String"`, `:String`, `42`, `nil`, `true`, `false`, `int`, `void`, `(do String)` или `(if true String Object)`
- **THEN** генерация завершается ошибкой операнда типа `instance?` с исходной позицией

#### Scenario: Неизвестный класс
- **WHEN** форма использует отсутствующий на classpath тип `missing.package.NoSuchType`
- **THEN** `javac` отклоняет generated Java до исполнения программы

### Requirement: Eval и JavaScript SHALL явно отклонять instance?

JavaScript target SHALL отклонять компиляцию формы `instance?`, а eval SHALL отклонять её при достижении во время исполнения. Диагностика SHALL называть `instance?` и указывать, что форма поддерживается только Java target. На eval аргументы неподдерживаемой формы SHALL NOT вычисляться.

#### Scenario: JavaScript compilation
- **WHEN** JavaScript target компилирует функцию, содержащую `(instance? String value)`
- **THEN** компиляция завершается явной ошибкой неподдерживаемой формы

#### Scenario: Eval execution
- **WHEN** eval достигает `(instance? String (make-value))`
- **THEN** возникает явная ошибка неподдерживаемой формы до вызова `make-value`
