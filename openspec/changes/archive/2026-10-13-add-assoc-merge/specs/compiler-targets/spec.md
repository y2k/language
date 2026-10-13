# Spec Delta

## ADDED Requirements

### Requirement: Compiler targets SHALL предоставлять assoc и merge для map

JS и Java SHALL предоставлять без пользовательского импорта `(assoc map key value)` и `(merge left right)`. Assoc SHALL добавлять или заменять одно поле; merge SHALL объединять поля с приоритетом правой map. Результат SHALL быть новой поверхностной map без изменения входов. Строковые и keyword ключи SHALL давать те же результаты, что eval. Порядок обхода и равенство map между targets не гарантируются.

#### Scenario: Добавление и замена
- **WHEN** m равно `{:a 1}` и выполняются `(assoc m :b 2)` и `(assoc m :a 3)`
- **THEN** результаты содержат соответственно a=1,b=2 и a=3 с count=2 и count=1
- **AND** m сохраняет a=1 и count=1

#### Scenario: Объединение и пустые map
- **WHEN** выполняются `(merge {:a 1 :b 2} {:a 3 :c 4})`, `(merge {} {:a 1})`, `(merge {:a 1} {})` и `(merge {} {})`
- **THEN** результаты содержат соответственно a=3,b=2,c=4; a=1; a=1; пустую map
- **AND** оба входа сохраняют свои поля и значения

#### Scenario: Вызов во вложенном namespace
- **WHEN** source в namespace `checks.maps.core` вызывает assoc и merge
- **THEN** generated JS и Java компилируются с обновлённым runtime и возвращают ожидаемые поля без пользовательских imports

### Requirement: Compiler assoc и merge SHALL сохранять значения поверхностно

Assoc и merge SHALL сохранять значения false, nil, 0 и пустую строку без фильтрации по истинности. Вложенные map SHALL заменяться целиком и SHALL NOT копироваться рекурсивно. Значения объектов SHALL передаваться без преобразования. Эти правила SHALL совпадать с eval для переносимых значений.

#### Scenario: Перезапись ложными значениями
- **WHEN** вычисляются `(assoc {:x 1} :x nil)` и `(merge {:a 1 :b 2} {:a false :b nil})`
- **THEN** count результатов равен 1 и 2, соответствующие поля равны nil, false и nil, а не строкам

#### Scenario: Вложенная map заменяется целиком
- **WHEN** r равно `(merge {:nested {:a 1 :b 2}} {:nested {:a 3}})`
- **THEN** nested содержит только a=3 и count=1

#### Scenario: Ссылочные значения сохраняются
- **WHEN** значение Atom проходит через assoc или merge и извлекается из результата
- **THEN** изменение извлечённого Atom наблюдается через исходную ссылку

### Requirement: Compiler targets SHALL ограничивать арности и map аргументы

Assoc SHALL принимать ровно три аргумента, merge — ровно два. Nil, list и скаляры вместо map SHALL отклоняться, а не трактоваться как пустые map. Неверная арность SHALL отклоняться при компиляции либо исполнении; единые стадия, класс и текст ошибки не требуются. Новые overloads для нескольких пар, нескольких map или векторов SHALL NOT предоставляться.

#### Scenario: Неверная арность
- **WHEN** source вызывает `(assoc {})`, `(assoc {} :a 1 :b 2)`, `(merge {})` или `(merge {} {} {})`
- **THEN** программа отклоняется при компиляции либо исполнении

#### Scenario: Неверная коллекция
- **WHEN** assoc получает nil, list или скаляр вместо map либо merge получает такое значение в любой позиции
- **THEN** исполнение завершается ошибкой, включая сочетания с пустой map

### Requirement: JavaScript assoc и merge SHALL поддерживать JSON-объекты

JS SHALL принимать map языка и обычные объекты из JSON.parse. Копирование SHALL включать собственные перечислимые строковые поля, без унаследованных полей. Результат SHALL иметь null prototype, а ключи `__proto__` и `constructor` SHALL оставаться обычными данными. Поддержка class instances, getters, proxies и symbol keys не входит в контракт.

#### Scenario: Обновление данных события
- **WHEN** item и result получены из JSON.parse и выполняется `(assoc (merge item result) :selection_reason (:reason item))`
- **THEN** результат содержит объединённые поля с правым приоритетом и selection_reason из item; оба исходника сохраняются

#### Scenario: Специальные строковые ключи
- **WHEN** JSON-объект с собственными полями `__proto__` и `constructor` передаётся merge либо assoc обновляет эти ключи
- **THEN** поля доступны как данные в результате, его prototype остаётся null
