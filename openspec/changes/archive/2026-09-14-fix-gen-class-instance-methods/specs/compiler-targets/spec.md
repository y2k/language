## ADDED Requirements

### Requirement: Java gen-class SHALL поддерживать новые void instance methods

Java target SHALL позволять объявлять через `gen-class` новые публичные `void` instance methods, отсутствующие у указанного предка, в пределах существующих ограничений арности и Java-типов. Метод SHALL передавать текущий экземпляр первым аргументом в соответствующую top-level реализацию `-method`, а объявленные аргументы — следом в исходном порядке. Методы `gen-class` SHALL генерироваться без `@Override`. Переопределение SHALL определяться обычными правилами Java, без новой metadata для проверки такого намерения. Non-void methods SHALL оставаться неподдерживаемыми.

#### Scenario: Новый метод у подкласса Object
- **WHEN** source объявляет `(gen-class :name Runner :extends Object :methods [[main [] void]])` и top-level `(defn -main [this] (println "ok"))`
- **THEN** generated Java компилируется через `javac` вместе с runtime
- **AND** вызов `main()` на экземпляре `Runner` выводит `ok`
- **AND** объявленный метод не содержит `@Override`

#### Scenario: Передача получателя и аргументов нового метода
- **WHEN** source объявляет новый метод `[accept [String String] void]` и реализацию `-accept` с параметрами `this`, `left`, `right`
- **THEN** вызов `accept("left", "right")` передаёт реализации тот же экземпляр, строку `"left"` и строку `"right"` в этом порядке

#### Scenario: Неподдерживаемый результат метода
- **WHEN** `gen-class` объявляет метод с return type, отличным от `void`
- **THEN** Java-генерация завершается ошибкой `gen-class: only void methods are supported`

### Requirement: Java gen-class SHALL сохранять независимость переопределения и вызова super

Метод `gen-class`, сигнатура которого переопределяет доступный метод предка по правилам Java, SHALL сохранять динамическую диспетчеризацию независимо от наличия `^override`. Без этой metadata generated method SHALL вызывать только соответствующую реализацию языка, без неявного вызова предка. При `^override` метод SHALL сначала вызвать `super.method(...)` с исходными аргументами, а после его нормального завершения — реализацию языка ровно один раз. Metadata SHALL NOT делать допустимым вызов отсутствующего или недоступного метода предка.

#### Scenario: Переопределение без super
- **WHEN** `gen-class` переопределяет конкретный `void`-метод предка без `^override`, и метод вызывают через ссылку типа предка
- **THEN** выполняется реализация языка ровно один раз
- **AND** тело метода предка не выполняется
- **AND** generated method не содержит `@Override`

#### Scenario: Переопределение с super
- **WHEN** `gen-class` переопределяет конкретный `void`-метод предка с `^override`, и вызов предка завершается нормально
- **THEN** сначала выполняется тело метода предка ровно один раз, затем реализация языка ровно один раз
- **AND** реализация языка наблюдает побочные эффекты уже выполненного метода предка
- **AND** generated method не содержит `@Override`

#### Scenario: Super для отсутствующего метода
- **WHEN** подкласс `Object` объявляет `[^override main [] void]`
- **THEN** generated Java содержит вызов `super.main()` и отклоняется `javac`, поскольку такого метода у предка нет

### Requirement: Java gen-class SHALL позволять прямой запуск instance main на JDK 25

Для namespace `checks.entry`, `gen-class` с именем `Runner`, предком `Object` и методом `[main [] void]` Java target SHALL генерировать вложенный класс, запускаемый непосредственно как `checks.entry$Runner` средствами JDK 25. После компиляции generated source вместе с runtime запуск SHALL выполнять реализацию `-main` без отдельного static-main wrapper и без preview flags.

#### Scenario: Прямой запуск вложенного класса
- **WHEN** source содержит `(ns checks.entry)`, `(gen-class :name Runner :extends Object :methods [[main [] void]])` и `(defn -main [this] (println "ok"))`, а generated Java и runtime скомпилированы в каталог `out` средствами JDK 25
- **THEN** команда `java -cp out 'checks.entry$Runner'` на JDK 25 успешно завершается и выводит `ok`
- **AND** запуск не требует `--enable-preview` или отдельного класса-обёртки
