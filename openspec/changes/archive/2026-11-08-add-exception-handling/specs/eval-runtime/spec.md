# Spec Delta

## ADDED Requirements

### Requirement: Eval SHALL явно отклонять try и throw

При достижении `try` или `throw` eval SHALL выдавать `Eval_error`, называющий форму и поддержку только JS/Java. Тело, обработчик, очистка и аргумент неподдерживаемой формы SHALL NOT вычисляться. Это SHALL NOT вводить представление пойманных исключений или их обработку в eval. Цитированные формы и корректные формы в недостигнутых ветвях SHALL сохранять существующее поведение.

#### Scenario: Try не исполняет секции
- **WHEN** eval достигает `(try (reset! state 1) (catch e (reset! state 2)) (finally (reset! state 3)))`
- **THEN** возникает явная ошибка неподдерживаемой формы `try`, а state не меняется

#### Scenario: Throw не исполняет аргумент
- **WHEN** eval достигает `(throw (reset! state 1))`
- **THEN** возникает явная ошибка неподдерживаемой формы `throw`, а state не меняется

#### Scenario: Недостигнутая форма
- **WHEN** eval исполняет `(if false (try 1 (finally nil)) "ok")` либо `(if false (throw nil) "ok")`
- **THEN** результат равен `"ok"`

#### Scenario: Цитирование
- **WHEN** eval читает `'(try)` или `'(throw)`
- **THEN** результатом являются данные, а не исполнение специальной формы
