# Spec Delta

## ADDED Requirements

### Requirement: Java target SHALL задавать явную видимость top-level определений

Java target SHALL генерировать обычные top-level `defn` и core `def`, содержащие `fn*`, как `public static` методы, а остальные обычные top-level `def` как `public static` поля. Определения с private metadata, включая `defn-` и `def-`, SHALL генерироваться соответственно как `private static` методы и поля. Правило SHALL применяться как при наличии `ns`, так и к классу `user` без `ns`, независимо от способа инициализации поля.

#### Scenario: Публичные и приватные методы
- **WHEN** исходник объявляет `(defn visible [] 1)` и `(defn- hidden [] 2)`
- **THEN** Java содержит `public static Object visible()` и `private static Object hidden()` с обычным контрактом исключений

#### Scenario: Поля с простым и составным значением
- **WHEN** исходник объявляет `(def scalar 1)`, `(def items [1 2])`, `(def- secret 3)` и `(def- secrets [3 4])`
- **THEN** поля `scalar` и `items` имеют модификаторы `public static`, а `secret` и `secrets` имеют модификаторы `private static`
- **AND** значения и порядок инициализации сохраняются

#### Scenario: Определения без namespace
- **WHEN** публичные и приватные определения компилируются без `ns`
- **THEN** методы и поля класса `user` получают те же явные модификаторы видимости

### Requirement: Java target SHALL обеспечивать публичный доступ между packages

Публичные методы и поля сгенерированного namespace SHALL быть доступны другому namespace через alias `:require`, включая namespace в другом Java package. Совместная компиляция исходников вместе с runtime через `javac` SHALL завершаться успешно, а исполнение SHALL возвращать объявленные значения.

#### Scenario: Вызов функции из issue #23
- **WHEN** `words.dic.serbian` объявляет `(defn words [] [["zdravo" "привет"]])`, а `words.app` требует его как `serbian` и вызывает `(serbian/words)`
- **THEN** сгенерированные классы компилируются вместе с runtime
- **AND** вызов возвращает объявленный вложенный список

#### Scenario: Чтение публичных полей
- **WHEN** namespace в другом package через alias читает публичные поля, одно с простым значением и другое со значением коллекции
- **THEN** generated Java компилируется, а чтение возвращает исходные значения обоих полей

### Requirement: Java target SHALL ограничивать private определения своим namespace

Приватные методы и поля SHALL оставаться доступными внутри сгенерированного класса своего namespace, включая вложенные классы `gen-class`. Обращение к ним из класса другого namespace SHALL отклоняться `javac`, даже если оба namespace отображаются в один Java package. Точный текст диагностики `javac` не фиксируется.

#### Scenario: Внутренний доступ
- **WHEN** публичная функция своего namespace вызывает `defn-` и читает `def-`
- **THEN** generated Java компилируется и функция возвращает ожидаемый результат

#### Scenario: Внешний доступ из того же package
- **WHEN** другой namespace того же Java package отдельно пытается вызвать приватную функцию или прочитать приватное поле
- **THEN** каждый такой исходник отклоняется `javac` из-за недоступности соответствующего члена

#### Scenario: Внешний доступ из другого package
- **WHEN** namespace другого Java package отдельно пытается вызвать приватную функцию или прочитать приватное поле
- **THEN** каждый такой исходник отклоняется `javac` из-за недоступности соответствующего члена

#### Scenario: Приватная реализация gen-class
- **WHEN** метод `gen-class` делегирует в `defn-` своего namespace
- **THEN** generated Java компилируется, а вызов метода выполняет приватную реализацию
