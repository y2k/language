# Tasks

## 1. Матрица контрактов stdlib

- [x] 1.1 Проверить реализованную основу `separate-eval-scalar-types` по typed scalar и numeric-key сценариям; добавить `test/eval_stdlib_contracts_test.ml` и зарегистрировать в `test/dune`, покрыв каждый binding из матрицы корректными границами и неверными типами/арностями; проверить соответствие строк таблицы `Eval_stdlib.env`.
- [x] 1.2 Дополнить локальные проверки stdlib по матрице без новых overloads и text-to-number coercion; проверить положительные variadic/unary cases, пустой reduce без init, integer-only API и все отрицательные cases целевого suite.
- [x] 1.3 Обновить английский справочник матрицей eval-арностей и строгими типами; проверить сохранение portable binary =/not= в общей части и документирование variadic поведения только для eval.

## 2. Callbacks и единый lookup

- [x] 2.1 Проверять callable в map/reduce/swap! до обхода; добавить случаи пустой/одноэлементной коллекции, отсутствие вызова допустимого callback, неверную callback-арность при фактическом вызове и проброс ошибок callback; проверить имя внешнего builtin только для его собственной ошибки аргументов.
- [x] 2.2 Добавить regression cases для get/get-in/let/fn lookup из второго change, включая эквивалентные числовые ключи, первый дубликат, раздельные scalar types и ключи-функции/Atom; проверить одинаковые результаты всех способов доступа без повторной реализации equality.
- [x] 2.3 Уточнить callback и map-key контракты справочника, дополнить существующие Atom-тесты при необходимости; проверить отсутствие финальной записи swap! при ошибке и сохранение побочных эффектов callback.

## 3. Ошибки операций и runner

- [x] 3.1 Добавить явные проверки отрицательного list index и нулевого делителя, узко нормализовать ошибки пути slurp; проверить Eval_error и диагностическую причину в целевом suite, включая пустой list, нулевой делитель после успешного шага и NUL в пути, поданный из OCaml-теста.
- [x] 3.2 Проверить runner и CLI на ошибках арности/типа/index/division/file без глобального перехвата всех исключений; добавить regression cases для Error result, stderr/ненулевого статуса и сохранения точных сообщений assert/hash-map/slurp.
- [x] 3.3 Обновить eval-раздел справочника с определёнными ошибками индекса/деления и ограничением переносимости; проверить, что успешные `(/ 0)`, truncation к нулю, drop <= 0 и get nil сохраняются в тестах.

## 4. Интеграционная проверка

- [x] 4.1 Выполнить `make test`; проверить suites eval, JS и Java, включая fractional-arithmetic, nil, boolean, scalar и Atom regressions.
- [x] 4.2 Сверить delta с предыдущими двумя changes без отката их требований и выполнить `openspec validate enforce-eval-stdlib-contracts --strict`; проверить отсутствие незаполненных пунктов проектирования и сохранение всех прежних scenario names изменённых требований.

## Проверки реализации

- Baseline: `make test` прошёл на чистом checkout после scalar-change. Матрица покрывает каждый binding `Eval_stdlib.env`, что отдельно проверяет suite.
- RED: 15 сбоев отдельных callback/index/division/runner проверок до изменения stdlib; прежние scalar и новые lookup/portable fixtures проходили.
- GREEN: `make test` после правки и форматирования прошёл: 22 проверки contracts, 216 samples и все остальные suites eval/JS/Java.
- `slurp` уже нормализует недопустимые пути через локальный `Sys_error`: тест с настоящим NUL, сформированным в OCaml, проходил до изменения runtime. Существующий узкий обработчик сохранён; regression проверяет точное сообщение, runner и реальный CLI.
- Runner/CLI проверены процессом `../bin/main.exe`: Error, пустой stdout, stderr с сообщением и ненулевой код для ошибок арности, типа, индекса, деления и файла. Callback effects проверяются на самом Atom и отдельном счётчике; ранняя проверка callable не меняет eager evaluation аргументов.
- Справочник содержит отдельную eval-матрицу; общие binary equality и ограничения переносимости сохранены. Два MODIFIED-блока delta сохраняют все текущие scenario names; четыре ADDED-блока не заменяют boolean/scalar requirements. Строгая OpenSpec-валидация, `git diff --check` и `ocamlformat --check` прошли.
- Независимое read-only review: замечаний нет. Non-finite/overflow, registry signatures, предварительная проверка арности callback, изменения JS/Java и regex остаются явно заявленными Non-Goals; отклонений от согласованных контрактов нет.
