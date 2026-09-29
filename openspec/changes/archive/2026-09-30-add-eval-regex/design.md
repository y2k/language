# Design

## Context

См. `proposal.md`, раздел Why. Пользователь выбрал Re и подтвердил Re.Perl. Подготовительные changes уже архивированы: eval различает String/Symbol/Int/Float/Bool/Nil, проверяет stdlib contracts и использует общий `equal_value`/lookup.

Исследованный baseline:
- `Eval_types.value` — закрытый variant; `truthy` уже считает истинным всё, кроме Nil/Bool false.
- `Eval_stdlib.env` хранит обычные native closures; `equal_value` возвращает false для непрозрачных типов, `to_string` и `Eval.value_kind` перечисляют конструкторы явно.
- Runner выводит только String/Symbol/Int/Float/Bool/Nil, прочие результаты дают `Ok ""`; Eval_error уже преобразуется в Error.
- Матрица `test/eval_stdlib_contracts_test.ml` проверяет точное соответствие всех строк bindings из env; новые bindings потребуют новых строк даже при отдельном regex suite.
- Re установлена в текущем opam switch, но не объявлена зависимостью проекта. В доступных интерфейсах есть `Re.Perl.compile_pat`, `Re.exec_opt`, `Re.Group.get` и `Re.replace_string`. Чтение `perl.ml` подтвердило default anchors начала/конца всей строки, отсутствие inline flags и исключения Parse_error/Not_supported; `replace.ml` уже обеспечивает конечный обход пустых совпадений и буквальную вставку replacement.
- В `prelude/language_runtime.js` уже есть host Regex и regex-функции, хотя `backend_compiler/js.ml` не включает их в стандартный runtime import. Эта существующая реализация не является основанием обещать переносимость; её не нужно удалять или изменять ради eval-change. Для сборочного fixture JS emitter выдаёт начальную строку runtime import и `export const`, а runtime содержит `export function`.

## Goals / Non-Goals

**Goals:** минимальное расширение существующего eval через три native bindings; собственный тип regex, повторное использование compiled matcher, локальная нормализация ожидаемых ошибок; воспроизводимая текстовая сборка небольшого userscript из файлов.

**Non-Goals:** свой regex engine, parser или цикл замены; общий registry signatures; автоматическое приведение аргументов; Unicode/codepoint matching, дополнительные flags-аргументы, captures API, callback replacement, literal reader syntax; анализ произвольного JavaScript и полноценный bundler; изменения реализации JS/Java.

## Decisions

### 1. Прямое использование Re.Perl

Объявить `re = 1.13.2` в `dune-project` и перегенерировать `language.opam` через Dune; добавить `re` к libraries в `backend_eval/dune`. Использовать `Re.Perl.compile_pat` с default options. Пользователь выбрал этот синтаксис вместо POSIX; Str требует другого синтаксиса, а PCRE bindings добавили бы ненужную native-зависимость и не соответствуют выбору Re.

После review пользователь согласовал устранение дефекта зависимости. В Re 1.14.0 `[\W]` ошибочно использует word class без complement. Стабильная [Re 1.13.2](https://github.com/ocaml/ocaml-re/blob/1.13.2/lib/perl.ml) не содержит этой регрессии и сохраняет все согласованные контракты. В upstream дефект устранён коммитом `4d3240e0aed5c055260c6f73a78aae22053a0e64`, но текущий master включает также изменение default `$` (`8d3194923759fcd9ae37b0dbe32e32ace5441749`). Поэтому фиксируется проверенный релиз 1.13.2 вместо непроверенного master или собственного vendor patch. Подъём версии требует проверки regex suite, особенно `[\W]`, `$` и zero-width replacement. Разработчику в существующем switch: `opam install re.1.13.2`.

Справочник перечисляет проверенные конструкции и ограничения: case-sensitive, dot не включает newline, anchors относятся ко всей строке, byte-based matching. Для multiline extraction доступен класс `[\s\S]` с обычными правилами строкового экранирования языка; не предлагать `(?m)`/`(?s)`. Не обещать полную совместимость с Perl/Java Pattern. Backreferences и lookaround недоступны. Проверить эти примеры тестами, прежде чем публиковать справочник.

### 2. Непрозрачный `Regex of Re.re`

Добавить один конструктор, без хранения исходного шаблона, identity token или глобального cache. Compiled value хранится в существующих bindings и коллекциях. Проектное решение для нового типа: равенство как у Closure/Atom — false даже при сравнении с собой; поэтому regex-ключи не находятся. Альтернатива — сравнение исходных шаблонов — требует дополнительного хранения и отдельной семантики flags, не нужной для issue. Это не новый скаляр и не изменение числового равенства.

`to_string` получает `#<regex>`, `value_kind` — `regex`. Truthiness и финальный output runner уже подходят, но нужны проверки. Не применять OCaml structural equality к compiled regex в тестах: проверять конструктор, результаты поиска и языковое равенство. `quote` не создаёт regex; missing data остаются Nil. Ассоциативные шаблоны не получают нового синтаксиса regex-ключа; regex как значение передаётся без изменений.

### 3. Три небольших builtin без новой подсистемы

- `re-pattern`: pattern match на `[String pattern]`, компиляция; локально преобразовать только `Re.Perl.Parse_error` и `Re.Perl.Not_supported` в Eval_error с именем операции и причиной. Точный текст остальных новых ошибок не является контрактом.
- `re-find`: `[Regex regex; String text]`, `Re.exec_opt`; `Some groups` превращается в `String (Re.Group.get groups 0)`, `None` — Nil. Наличие capture groups не меняет форму результата.
- `re-replace`: `[String text; Regex regex; String replacement]`, `Re.replace_string ~all:true regex ~by:replacement text`. Этот API вставляет строку буквально, обрабатывает пустые совпадения и не сканирует вставленный текст. Свой цикл, capture substitution или callback не нужны.

Для прочих аргументов — локальные ошибки expected arguments. Regex не принимается как callback даже на пустой коллекции. Ошибки runner не требуют catch-all; не преобразовывать неожиданные ошибки движка в ложное отсутствие совпадения.

### 4. Тестирование по слоям

- Дополнить матрицу contracts тремя строками: положительные результаты проверять через re-find/str, неправильные типы на каждой позиции и неверные арности, включая empty text и nil. Проверить, что keywords остаются строками.
- Добавить `test/eval_regex_test.ml`, зарегистрировать в `test/dune`: constructor checks; свойства непереносимого regex runtime; границы диалекта, nil против пустой строки; буквальные dollar/backslash; zero-width progression; повторное использование regex после успешного и неуспешного поиска. Проверить типы и результат, а не только одинаковый текст.
- Положительные языковые примеры — `test/samples/eval/regex_*.clj`, с проверками через str и бинарные =/not=. Они eval-only; общие samples остаются проверяемыми на всех targets.
- Runner/реальный CLI: invalid pattern/type/arity, stderr и exit; итоговый regex даёт пустой результат, str даёт маркер. Использовать существующую Dune-зависимость на `../bin/main.exe`.
- Интеграционный случай issue: получить JS небольшого примера через существующий compiler runner, взять реальный JS runtime и небольшой userscript header, сохранить во временные файлы. Eval-программа читает их через slurp, извлекает header, удаляет конкретные module directives и объединяет текст. Проверить порядок/содержимое, отсутствие выбранных directives и выполнить результат в Node для наблюдаемого результата. Файлы и cwd изолировать/убрать через Fun.protect. Это ограниченный fixture, не обещание корректной обработки любого JS регулярками.
- Для этого fixture достаточно удаления начального runtime import шаблоном `^import [^\n]*\n` и буквальной замены `export ` на пустую строку в известных входах без такого текста внутри JS strings. Header извлекается по delimiters с `[\s\S]*?`. В программном source экранировать backslash согласно существующему frontend; не требовать inline multiline flags.

### 5. Согласование спецификаций и справочника

Delta содержит только ADDED requirements внутри `eval-runtime`: текущий перечень stdlib явно допускает bindings других требований, а scalar runner уже имеет закрытый список печатаемых категорий. Поведение новых regex описано отдельными требованиями, не переписывающими предшествующие boolean/scalar/contracts. Добавить три строки в eval-матрицу справочника, раздел синтаксиса/ограничений и пример сборки; portable binary equality и JS/Java разделы не расширять regex-гарантиями.

## Risks / Trade-offs

- Название Perl создаёт ложные ожидания flags/Unicode/backreferences → точные ограничения и отрицательные тесты.
- Zero-width замена может неожиданно отличаться от другого движка → следовать Re и закрепить `""`/`"a*"`/anchors тестами; не писать свой обход.
- Re matcher содержит внутреннее состояние → не сравнивать OCaml-структуры; тестировать повторное использование и непрозрачное языковое равенство.
- Разные версии Re могут менять диалект → зафиксировать проверенную 1.13.2 в package metadata; версия выбрана для устранения подтверждённой регрессии 1.14.0, не по случайному локальному окружению.
- Новый конструктор влияет на диагностику и stringification → сборка выявляет неэкзостивные matches; тесты охватывают передачу, потребителей, коллекции, Atom, quote, Nil и runner.
- Regex-замена module directives не является JS parser → интеграционный fixture и документация ограничивают применение известным форматом входных файлов.

## Migration Plan

Добавить зависимость, runtime, API, проверки и справочник одной реализацией; выполнить `make test` и строгую OpenSpec-валидацию. Существующим программам миграция не нужна: API добавочный, прежние bindings и типы не меняются. Откат удаляет только regex API/конструктор и зависимость; три подготовительных changes сохраняются. Архивация синхронизирует новые требования, не накладывая старые nil/Atom deltas поверх актуальных контрактов.
