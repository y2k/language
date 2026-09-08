## MODIFIED Requirements

### Requirement: The project SHALL keep unclear current behavior separate from desired changes

Эта спецификация фиксирует только наблюдаемую неоднозначность. Она SHALL NOT рассматриваться как запрос на изменение production-поведения.

#### Scenario: Top-level `def` convention versus evaluator support
- **GIVEN** соглашения проекта допускают `def` только на верхнем уровне
- **AND** pattern matching evaluator позволяет выполнять `(def ...)` везде, где вычисляется форма
- **WHEN** документируется семантика языка
- **THEN** поведение вложенного `def` остаётся неопределённым и на него нельзя полагаться без явного решения

#### Scenario: JavaScript namespace imports ignore Java imports
- **GIVEN** макрос `ns` записывает requires и imports
- **WHEN** JavaScript compilation обрабатывает `compiler/ns`
- **THEN** require-пары создают imports, а Java imports не имеют документированного эффекта для JavaScript

#### Scenario: Type annotations outside Java lambdas
- **GIVEN** parser metadata сохраняет `^TYPE` annotations
- **AND** Java compiler использует annotations для lambdas и override methods в `gen-class`
- **WHEN** annotations находятся на defs, let bindings или parameters
- **THEN** их предполагаемый runtime/compiler эффект остаётся неясным за пределами сохранения metadata

#### Scenario: Numeric model
- **GIVEN** определение чисел в parser/compiler принимает floats через `float_of_string_opt`
- **AND** арифметика eval преобразует значения через `int_of_string_opt`
- **WHEN** десятичные числа используются в арифметике eval
- **THEN** числовое поведение между targets остаётся неясным

#### Scenario: Hash map ordering and duplicate keys
- **GIVEN** hash maps представлены association lists
- **WHEN** maps содержат повторяющиеся ключи или преобразуются в строки
- **THEN** точная семантика повторяющихся ключей и гарантии порядка остаются неясными за пределами текущего поведения списков

#### Scenario: Java function arity limit
- **GIVEN** Java function interfaces существуют для арностей 0, 1 и 2
- **WHEN** top-level функция, компилируемая в Java, имеет больше двух аргументов
- **THEN** Java compilation завершается ошибкой, но предполагаемое ограничение арности на уровне языка остаётся неясным

#### Scenario: Parser string escape semantics
- **GIVEN** `frontend-syntax` определяет декодирование `\"`, `\\`, `\n`, `\t`, `\r` и буквальное сохранение остальных escape-пар
- **WHEN** исходная строка не закрыта или обрывается после обратного слеша
- **THEN** точная диагностика некорректно завершённой строки остаётся вне принятого контракта; семантика полных escape-пар больше не является открытым вопросом
