## ADDED Requirements

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
