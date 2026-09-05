## Context

Frontend раскрывает built-in macros до рекурсивного обхода дочерних форм, а все targets уже реализуют последовательный `let*`, ленивый `if` и общую truthiness-семантику. Требования определены в `specs/frontend-syntax/spec.md`; мотивация приведена в `proposal.md`.

## Goals / Non-Goals

**Goals:**

- Реализовать последовательный short-circuit только средствами существующих core forms.
- Проверять структуру `if-let` во время macro expansion.
- Сохранить одинаковое поведение в `eval`, JavaScript и Java без backend-specific логики.

**Non-Goals:**

- Destructuring binding forms, несколько выражений в одной ветке или отдельный AST-узел.
- Scope-анализ ссылок из `else` и гарантии доступности там bindings `if-let`.
- Изменение существующей truthiness-семантики `if` или механизма frontend-ошибок.

## Decisions

Макрос SHALL быть зарегистрирован в существующем списке `Builtin_macros.builtin_macros`. Форма

```clojure
(if-let [user (find-user)
         id (get user "id")]
  id
  "missing")
```

будет концептуально раскрыта в:

```clojure
(let* (user (find-user))
  (if user
    (let* (id (get user "id"))
      (if id
        id
        "missing"))
    "missing"))
```

Каждая пара создаёт отдельный вложенный `let*`; единый `let*` для всех пар отклонён, потому что он вычислил бы поздние expressions до проверки ранних значений. Пользовательское binding name одновременно хранит результат и служит condition, поэтому generated temporary и `Gensym` не нужны.

Рекурсивное раскрытие завершается `then` после последней успешной пары. Отсутствующий `else` представляется явным `nil`. Исходный `else` используется в каждой failure branch, но во время выполнения выбирается не более одной из них.

Bindings принимаются только в непустом `Bracket`-списке с чётным количеством элементов. Binding name должен быть атомом, который не является string, number, keyword, `nil`, `true` или `false`; collection patterns и literals отклоняются. Parenthesized форма с head `if-let`, но неверной структурой или арностью, завершается существующей frontend macro-ошибкой вместо преобразования в обычный function call.

Bindings гарантированно доступны последующим RHS и `then`. `else` намеренно не получает scope-контракта: реализация не добавляет closure, временные bindings или отдельный анализ ссылок только для выравнивания видимости между failure branches. Программа, обращающаяся из `else` к имени `if-let`, находится вне специфицированного поведения.

## Risks / Trade-offs

- [Исходный `else` присутствует в нескольких ветвях раскрытого AST] -> Проверить ленивость общей cross-target sample-программой; отдельная runtime-ветвь выполняется только один раз.
- [Обращение к binding из `else` может проявляться по-разному в targets] -> Явно исключить такое обращение из контракта и не добавлять несоразмерный scope-анализ.
- [Классификация symbols дублирует различение literals, потому что AST хранит их как `SAtom`] -> Использовать небольшой локальный predicate; не менять AST и parser ради одного макроса.
- [`if-let` наследует существующую truthiness реализацию `if`] -> Не исправлять отдельные расхождения runtime в этом change и проверять только уже заявленные `false`, `nil` и обычные truthy-значения.
