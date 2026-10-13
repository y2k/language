# Design

## Context

Мотивация — `proposal.md`, раздел Why. Изменение затрагивает три runtime и подключение JS runtime, поэтому технические решения фиксируются здесь.

Eval уже различает `String`, `Symbol`, числовые значения, Bool и Nil. `vector?` в `backend_eval/eval_stdlib.ml` показывает шаблон unary-предиката и регистрации builtin. JS и Java предоставляют аналогичные `vector_QMARK_`; JS compiler перечисляет runtime imports явно, Java использует static wildcard import. Regex API уже проверяет строки через `typeof ... === "string"` и `instanceof String`.

## Goals / Non-Goals

**Goals:** использовать обычный вызов и существующий symbol munging, минимально расширив stdlib/runtime; согласовать предикат с типами, принимаемыми строковыми API.

**Non-Goals:** новые runtime-типы, выравнивание quote между targets, общая система проверки типов, `nonblank?`, расширение Java-ссылок на builtin как first-class значения и единая диагностика неверной арности на compiled targets.

## Decisions

### 1. Предикат в runtime, без специальной формы

- Eval: добавить unary builtin с ветками `String` / другое значение / неверная арность и binding `string?` по образцу `vector?`.
- JS: экспортировать `string_QMARK_` с проверкой примитивной строки и добавить имя в существующий список импортов `backend_compiler/js.ml`.
- Java: добавить `public static Boolean string_QMARK_(Object value)` с нативной проверкой `instanceof String` рядом с `vector_QMARK_`.

Обычный путь вызова сохраняет однократное вычисление аргумента. Альтернатива — специальная форма или макрос — добавила бы ветви в compiler без необходимости. Изменений AST, lowering и Java generator не требуется.

### 2. Проверять представление значения, не его происхождение

Согласованный контракт зафиксирован в двух delta specs. Keywords уже строки; quote сохраняет отдельные типы на eval, но генерирует текстовые атомы на JS/Java. Предикат отражает это различие. Проверка через JS Object.prototype.toString отвергнута: она принимает boxed String, который не принимает существующий regex API. Аналогично Java CharSequence шире нужного String и включил бы StringBuilder.

### 3. Арность по существующему шаблону vector?

Допустимый вызов имеет один аргумент. Eval проверяет это явно и выдаёт Eval_error; Java использует unary-сигнатуру. Для JS не добавлять отдельную проверку числа аргументов: поведение неверных вызовов compiled targets вне переносимого контракта. Это сохраняет объём обычного предиката, не вводя новый механизм диагностики.

### 4. Проверять через существующие suites

Общий `test/samples/string_predicate.clj` проверяет матрицу значений, boolean-результат через бинарное `=`, условие, значение из коллекции и однократный эффект аргумента. Sample с вложенным namespace проверяет подключение runtime. Результаты выводятся через `str` с ожидаемым комментарием первой строки.

Целевые samples в `test/samples/eval/`, `js/`, `java/` проверяют quote; JS sample также проверяет JSON.parse, boxed String, undefined и обычный объект, Java sample — String и StringBuilder. В существующую матрицу `test/eval_stdlib_contracts_test.ml` добавить boolean-конструкторы результата и нулевую/лишнюю арность. Полная проверка — `make test`.

В `skills/y2k-language/SKILL.md` добавить строку API и описать keywords, quote, host-типы и eval-арность на английском.

## Risks / Trade-offs

- Ожидание Clojure-семантики keywords/quote → явно документировать фактические типы и закрепить различия целевыми samples.
- Новый безусловный JS import несовместим со старой копией runtime → обновлять compiler и prelude вместе; `make build` уже копирует runtime в настроенный каталог пакетов.
- Печатное `false` может скрыть строку вместо boolean → проверять бинарное равенство, ветвление и runtime-конструктор в eval.

## Migration Plan

При реализации поставить обновлённые compiler и runtime совместно обычным процессом сборки; отдельная публикация пакета не входит в change. Существующие исходники не требуют миграции. Приложение events сможет заменить host-проверку на `(string? value)` после обновления prelude. При откате откатить compiler/runtime вместе и восстановить прежнюю проверку в использующем новый API приложении.
