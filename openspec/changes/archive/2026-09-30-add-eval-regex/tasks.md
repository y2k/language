# Tasks

## Журнал выполнения

- Baseline: `59a2750`; выполнение в текущем checkout по ранее согласованному выбору пользователя. Spec/design прочитаны, CLI apply — ready, 0/11.
- Связи задач: 1.2 даёт Regex для 2.1; 2.1 даёт bindings для 2.2–3.2; env-матрица должна обновляться вместе с bindings. Контракты согласованы; фактическое имя диагностического helper в baseline — `value_text`, а не `value_kind` из design.
- Проверки нового типа через язык будут написаны до API и завершены вместе с 2.1; учёт задач остаётся раздельным. Полные проверки — `make test`; основной итоговый review выполняется после всех задач.
- 1.1: `dune build` прошёл; Dune генерирует `_build/default/language.opam` с зависимостью `re` (файл в source tree не поддерживается проектом).
- RED: новые regex/матрица tests падали на отсутствующих bindings; отрицательный тест шаблонов уточнён, чтобы не принимать `symbol not found` за ошибку компиляции. GREEN: `make test` прошёл после добавления runtime/API — 6 regex cases, 22 contracts, 216 samples и остальные suites.
- 2.2/3.2: eval fixtures и сборка с настоящим JS runtime + compiler output прошли `make test`: 7 regex cases, 218 samples. CLI stdout собранного script запускается Node в CommonJS mode и печатает `42`; временные файлы удаляются через Fun.protect.
- Справочник до обновления проверен независимым read-only retrieval: regex API, порядок аргументов, empty-match/replacement и диалект в нём отсутствовали. Добавлены таблица и раздел с проверенными примерами.
- Финальные `make test`, `ocamlformat --check`, `git diff --check`, строгая OpenSpec-валидация прошли. Независимый review подтвердил полноту retrieval справочника и нашёл блокирующий дефект диалекта в установленной Re: `(re-find (re-pattern "[\\W]+") "!!!")` даёт nil, тот же шаблон для `"abc"` даёт `"abc"`; `"\\W+"` вне класса работает правильно. Воспроизведено реальным CLI: `nil|abc|!!!`. Источник — `/Users/igor/.opam/language/lib/re/perl.ml`, `let not_word = Set (Re.alt word_char)` вместо отрицания.
- Выполнение приостановлено на 4.1: изменение зависимости/её патч либо принятие исключения в диалекте требуют согласования пользователя. Никакая поддержка молча не исключена; исправление чужой библиотеки и ограничение API не внесены.
- Остальные границы review: JS/Java и general bundler явно вне scope; полный перебор синтаксиса Re, performance и совместимость всех прошлых версий не доказаны. Это не основание игнорировать воспроизведённый дефект. Результаты suites подтверждены исполнителем, reviewer их повторно не запускал.
- Блокер снят по выбору пользователя «1»: закреплена стабильная Re 1.13.2 без регрессии `[\W]`. Собственная копия/патч библиотеки не нужны. В локальном switch установлена 1.13.2; opam пересобрал alcotest/ocamlformat и установил seq.base. Сгенерированный `_build/default/language.opam` содержит точное ограничение версии.
- Review fix RED→GREEN: `non-word classes exclude word characters` падал на 1.14.0, прошёл на 1.13.2; покрывает положительный/отрицательный поиск, replacement, отрицание класса и смешанный класс. `make test` на 1.13.2 прошёл: 8 regex cases, 22 contracts, 218 samples и остальные suites всех targets, включая `$` и zero-width контракты.
- 4.1 завершён: после форматирования повторный `make test`, `ocamlformat --check`, `git diff --check` и `openspec validate add-eval-regex --strict` прошли. JS/Java implementation не изменены, предыдущие scalar/boolean/contracts сохранены. Единственное замечание review устранено регрессионным тестом и закреплением зависимости; отклонённых замечаний нет.

## 1. Зависимость и first-class regex

- [x] 1.1 Объявить `re` в `dune-project` и `backend_eval/dune`, перегенерировать `language.opam`; проверить `dune build` и наличие зависимости в генерируемом package metadata.
- [x] 1.2 Добавить `Regex of Re.re`, `#<regex>` в stringification и `regex` в диагностику; создать `test/eval_regex_test.ml` и зарегистрировать в `test/dune`. Проверить конструктор, truthiness, непрозрачное равенство/lookup (включая вложенные коллекции), финальный runner result и отклонение прямого вызова/использования как callback, без structural comparison Re.re; выполнить `make test`.
- [x] 1.3 Описать непрозрачный regex в eval-разделе `skills/y2k-language/SKILL.md`: передача, равенство, строковое представление и runner. Сверить утверждения с целевыми тестами; сохранить portable equality и ограничения targets.

## 2. Компиляция, поиск и замена

- [x] 2.1 Реализовать re-pattern/re-find/re-replace как три native bindings через Re.Perl.compile_pat, Re.exec_opt/Group.get 0 и Re.replace_string ~all:true; локально нормализовать Parse_error/Not_supported. Добавить три строки в `test/eval_stdlib_contracts_test.ml`, покрыть арности, неверный тип каждой позиции, symbols/nil/boolean/numbers, keywords как строки, empty text и отсутствие неявной компиляции; проверить соответствие всей матрицы env и выполнить `make test`.
- [x] 2.2 Добавить regex tests и `test/samples/eval/regex_*.clj` для первого полного совпадения с группами, nil против пустой истинной строки, greedy/lazy/alternation, регистра/anchors/dot/newline, escaped patterns, ошибочных и неподдерживаемых шаблонов. Проверить byte-based поведение и повторное использование compiled regex после поиска/замены; выполнить `make test`.
- [x] 2.3 Закрепить глобальную неперекрывающуюся замену, удаление/отсутствие совпадения, буквальные dollar/backslash, отсутствие повторного поиска в replacement и zero-width случаи из delta (включая пустой input и anchors); проверить точные результаты и завершение через целевой suite и `make test`.
- [x] 2.4 Проверить сохранение regex через def/let/fn, list/map, get/get-in, последовательную и ассоциативную деструктуризацию let/fn, Atom/deref/reset!/swap!; добавить quote/missing-data cases, убедиться в пригодности переданного regex для поиска и выполнить `make test`.
- [x] 2.5 Дополнить английский справочник тремя eval-арностями, диалектом Re.Perl/default options, ограничениями flags/lookaround/backreferences/Unicode, буквальной заменой и zero-width правилами. Проверить все примеры целевыми fixtures; сопоставить формулировки с реализованными тестами и не обещать Java Pattern/JS совместимость.

## 3. Runner, CLI и сборочный сценарий

- [x] 3.1 Добавить проверки runner и реального CLI для неверных арностей/типов, Parse_error/Not_supported, успешного re-find/re-replace, финального regex и явного str; проверить Error, stderr/ненулевой exit без host-исключений и scalar output, используя существующую зависимость на ../bin/main.exe; выполнить `make test`.
- [x] 3.2 Добавить интеграционный тест issue #24: компиляция небольшого примера в JS, временные файлы header/runtime/compiled JS, сборка eval-программой через slurp/re-find/re-replace/str, проверка stdout и запуск собранного script в Node с ожидаемым результатом. Документировать проверенный пример для этих форматов входа и shell redirection; выполнить `make test`, проверить очистку временных файлов и отсутствие зависимости сборочного скрипта от sed/Python/bundler.

## 4. Интеграционная проверка

- [x] 4.1 Выполнить полную `make test`, форматирование изменённых OCaml-файлов, `git diff --check` и `openspec validate add-eval-regex --strict`; проверить suites всех targets, соответствие справочника контрактам и отсутствие изменений JS/Java реализации либо отката boolean/scalar/stdlib contracts. Зафиксировать результаты проверок в change.
