## ADDED Requirements

### Requirement: Java compiler SHALL honor type hints on let bindings

Java target SHALL использовать `^TYPE` metadata символьного `let` binding для приведения RHS к `TYPE` перед созданием локальной переменной. RHS SHALL вычисляться ровно один раз в обычном последовательном порядке `let`, а последующие Java interop expressions SHALL видеть локальную переменную с указанным Java-типом. Это требование не применяется к destructuring patterns и не меняет поведение eval или JavaScript targets.

#### Scenario: Вызов Java-метода через hinted binding

- **WHEN** source содержит `(let [^java.lang.String text (make-text)] (.concat text "!"))`, где `make-text` компилируется с Java return type `Object`
- **THEN** Java output приводит результат `make-text` к `java.lang.String` перед созданием `text`
- **AND** generated Java компилируется и вызов `concat` возвращает ожидаемую строку
