# Tasks

## 1. Подключение API и переносимый поиск

- [x] 1.1 Добавить `re_pattern`, `re_find`, `re_replace` в стандартный import `backend_compiler/js.ml`; добавить общие fixtures для прямых вызовов и целевую проверку вложенного namespace. Проверить выполнение через Node/JVM и совпадение общих результатов с eval командой `make test`.
- [x] 1.2 Добавить `test/samples/regex_search.clj` с бинарными сравнениями через str: группы, альтернативы, greedy/lazy и bounded quantifiers, классы и shorthand classes внутри/вне скобок, nil/пустое совпадение, регистр, LF, anchors и escaped/class dollar. При необходимости исправить Java-нормализацию anchors; все cases должны проходить на трёх targets через `make test`.
- [x] 1.3 Обновить regex-раздел `skills/y2k-language/SKILL.md`: доступность на всех targets, арности, порядок аргументов, переносимая область (печатный ASCII, TAB, LF), native engines и расширения/Unicode вне гарантии. Проверить соответствие примеров общим fixtures и сохранить описание eval-only ограничений.

## 2. Буквальная глобальная замена

- [x] 2.1 Добавить общие fixtures replacement: несколько совпадений, отсутствие, удаление, `$1`/`$&`/backslash, неперекрываемость, отсутствие повторного поиска вставок, пустой pattern/text/replacement, `a*` в `ab` и `a`, anchors и повторное использование regex. Зафиксировать расхождение zero-width поведения текущих compiled runtimes с ожидаемым eval-результатом.
- [x] 2.2 Заменить native replacement traversal в JS и Java на явный обход по design.md с локальным matcher, сохранением исходных символов, пропуском пустого совпадения после непустого и гарантированным завершением. Проверить все replacement fixtures через `make test` с ограничением времени запуска и сохранить успешный userscript integration.
- [x] 2.3 Обновить справочник: общие правила буквальности/пустых совпадений, примеры `-a-b-` и `-b-`, различие единиц текста вне portable subset и изменение JS/Java поведения. Проверить примеры по passing fixtures.

## 3. Непрозрачное значение и прямое равенство

- [x] 3.1 Добавить regex guard для непосредственных операндов `_EQ_` обоих runtimes; проверить бинарные =/not= с собой, другим regex и строкой с обеих сторон общими fixtures и `make test`, включая существующие scalar equality tests.
- [x] 3.2 Добавить общие fixtures истинности, str (включая list/map values), передачи через функцию, get/get-in, let/fn destructuring и Atom, повторного использования после поиска/замены. Проверить сохранение regex через успешный re-find на всех targets, а не только текстовый вывод.
- [x] 3.3 Уточнить справочник: общий контракт прямого бинарного равенства и непрозрачности; отдельно обозначить eval-only recursive equality, regex keys, unary equality и runner output. Сверить каждую общую гарантию с fixtures и спецификацией.

## 4. Контракты и ошибки compiled programs

- [x] 4.1 Создать `test/regex_targets_test.ml` и зарегистрировать в `test/dune`, используя подход `test/get_in_test.ml`. Проверить недостаточную/избыточную арность каждого builtin, неверные типы всех позиций (включая nil, числа, booleans, коллекции), callback, malformed pattern и случай единственного nil у Java varargs; каждый generated source обязан успешно компилироваться, а вызов — давать runtime error с именем builtin и причиной.
- [x] 4.2 Подтвердить раннюю валидацию на пустом text и при отсутствии совпадения, отклонение прямого вызова regex и stderr/nonzero exit для необработанной ошибки в Node/JVM. Исправить только необходимые runtime guards; проверить новый suite и `make test`, не считая compilation failure подтверждением runtime error.
- [x] 4.3 Обновить справочник ошибок: runtime-строки/keywords, отсутствие coercion чисел/boolean/nil, target-specific quoted symbols, ошибки исполнения generated programs и непереносимость native extensions. Проверить согласованность с отрицательными тестами; не обещать eval CLI error contract стадии компиляции.

## 5. Интеграционная проверка

- [x] 5.1 Выполнить `make test` после всех изменений: общие fixtures на eval/JS/Java, новый compiled suite и существующий eval regex/userscript suite должны пройти; записать результат в change.
- [x] 5.2 Выполнить formatter check для изменённых OCaml-файлов, `git diff --check` в language и packages и `openspec validate sync-regex-targets --strict`; проверить, что все сценарии delta имеют тестовое покрытие, а новые зависимости и изменения eval-семантики отсутствуют. Зафиксировать согласованную поставку двух runtime-файлов packages вместе с compiler/tests/docs language.

## Журнал проверок

- 2026-09-30: JS API RED — общий fixture выявил `re_find is not defined`; GREEN после добавления runtime import и обновления 11 ожидаемых import-строк в `js_ns_test.ml`. Поиск прошёл на eval/JS/Java; вложенный namespace проверен реальным Node/JVM исполнением.
- Replacement RED — JS и Java дали `--b-` и `--` для `a*` в `ab` и `a`; eval дал согласованные `-b-` и `-`. GREEN после локального обхода matcher; `make test` прошёл с лимитом 120 секунд.
- Equality RED — JS и Java дали неправильное self-equality/self-inequality; GREEN после проверки непосредственных regex-операндов. Проверки передачи и str прошли на всех targets.
- При подготовке fixture повторное имя x в соседних let выявило существующий конфликт generated bindings; fixture использует отдельные имена sequential/associative, сохраняя оба сценария передачи regex. Изменения lexical lowering не входят в эту правку.
- Матрица из 66 отрицательных вызовов проверена отдельно на JS и Java; оба compiled sources успешно компилируются. Необработанные ошибки арности, pattern и nil-типа дополнительно проверены по stderr и ненулевому коду процесса.
- Итоговый `make test` после форматирования — PASS. Общий sample suite содержит 227 запусков, новый regex-targets suite — 4 cases; существующий eval regex suite с userscript integration сохраняется. `ocamlformat --check backend_compiler/js.ml test/js_ns_test.ml test/regex_targets_test.ml`, `git diff --check`, `openspec validate sync-regex-targets --strict` — PASS.
- Независимый review: ошибок логики не выявлено; дополнительно проверены 374 сочетания pattern/text на eval/JS. Выявлен блокер поставки: `prelude/language_runtime.js` и `.java` — отслеживаемые symlinks в отдельный `/Users/igor/project/packages`, где regex API уже был незакоммичен до начала реализации. Изменения replacement/equality этой сессии также попали туда. Language diff не включает содержимое runtime. Перед завершением требуется согласовать companion scope для `packages/prelude/1.0.0/{js,java}/language_runtime.*`, сохраняя прежние пользовательские изменения; задача 5.2 остаётся открытой до решения.
- Замечание review о timeout: текущие проверки запускались с внешним лимитом 120 секунд; встроенного timeout в test harness нет. Требование ограниченного запуска выполнено на уровне invocation, изменение общего runner не выполнялось.
- Пользователь подтвердил companion scope packages. Проверен его полный diff: regex API/str, replacement/equality и существовавшая ранее правка truthiness строк. Proposal и design теперь указывают реальные пути, зависимость и порядок совместной поставки/отката; блокер scope снят. `git diff --check` в packages — PASS. Новых изменений кода после последнего полного `make test` нет; его результат относится к текущей паре compiler/runtime. Коммиты и публикация не выполнялись.
- Завершение по запросу пользователя: пять требований синхронизированы в main `compiler-targets`, все прежние требования сохранены; change архивирован. Повторные `make test`, строгая валидация всех 7 main specs и diff-check обоих репозиториев — PASS. Companion runtime commit: `y2k/packages@fe1fe4f`; language commit поставляется вместе с ним, ссылка на него также включается в сообщение закрытия `y2k/language#24`.
