# Tasks

## 1. Scalar-модель и границы значений

- [x] 1.1 Убедиться, что `add-eval-booleans` реализован: проверить наличие Bool и прохождение его сценариев; добавить String/Int/Float и оставить Symbol для цитированных имён, обновить literal lookup/quote, `str`, `slurp`, `deps`, диагностику и runner; проверить прямые конструкторы и вывод каждого scalar, сохранение keyword-строк и namespace lookup в целевых тестах.
- [x] 1.2 Адаптировать `last_symbol`/`check_symbol` и их callers в namespace/deps/Atom suites без потери проверок типов; добавить eval-only fixtures typed quote и round-trip через bindings, функции, коллекции и Atom; проверить отдельность `42`, `"42"`, `'foo`, `"foo"`, false и nil.
- [x] 1.3 Обновить scalar/quote/representation/CLI-разделы `skills/y2k-language/SKILL.md`; проверить, что описаны eval-only typed quote, неизменные keywords-as-strings и отсутствие гарантии сохранения записи `1.00`.

## 2. Числовые producers и consumers

- [x] 2.1 Перевести арифметику на Int/Float без промежуточного преобразования в текст, count — на Int, integer-only APIs — на Int-аргументы; проверить текущие fractional-arithmetic samples, нормализацию целых результатов/отрицательного нуля и точность повторного использования дробей.
- [x] 2.2 Добавить тесты отклонения числовых строк, symbols и float в integer-only APIs, а также нестроковых slurp/deps; проверить, что `(get [10 20] (+ 0.5 0.5))` успешен, но `(get [10 20] 1.0)` даёт Eval_error.
- [x] 2.3 Обновить арифметические и eval-only string-API контракты справочника; сверить примеры с тестами и убедиться, что дробное деление/порядок не объявлены поддерживаемыми.

## 3. Равенство и lookup ключей

- [x] 3.1 Обновить `equal_value` для категорий скаляров и точного смешанного Int/Float-сравнения; добавить tests `1 = 1.0`, неравенства разных категорий, рекурсивных коллекций и больших целых/границ машинного int без округления; проверить boolean-результаты и отсутствие epsilon-сравнения.
- [x] 3.2 Заменить полиморфный lookup map в get и деструктуризации общим lookup по языковому равенству; проверить числовые/строковые/символьные/boolean ключи, первый из дубликатов, get-in, let/fn patterns и отсутствие host-исключений для ключей-функций/Atom.
- [x] 3.3 Обновить equality и map-key разделы справочника: числовое равенство eval, сохранение порядка и дубликатов, неравенство функций/Atom; проверить согласованность примеров с eval fixtures и не приписывать mixed equality всем targets.

## 4. Интеграционная проверка

- [x] 4.1 Выполнить `make test`; проверить все три targets, включая прежние fractional-arithmetic, namespace, deps, nil, boolean и Atom scenarios.
- [x] 4.2 Перед синхронизацией сверить delta с интегрированным boolean-change и завершёнными nil/Atom, сохранить все scenario names и актуальное имя требования runner; выполнить `openspec validate separate-eval-scalar-types --strict` и проверить, что требования первого change не откатываются.

## Проверки реализации

- Baseline `make test` прошёл; рабочее дерево было чистым. Работа в текущем checkout по ранее выбранному пользователем варианту.
- Связанные задачи модели, producers/consumers и lookup выполнены совместно: новые конструкторы требуют согласованного обновления exhaustive matches и тестовых helpers.
- RED: `make test` до изменения runtime показал две scalar-регрессии eval ns, отклонение нестроковых deps и три новых eval fixtures; общий fixture на JS/Java прошёл.
- GREEN: `make test` после реализации прошёл: 27 eval ns, 6 deps, 212 samples, прежние Atom/instance?/get-in и остальные suites. Helpers проверяют String/Int/Bool напрямую; constructor tests проверяют Float/Symbol и точность binary64.
- Справочник сверён с тестами: typed quote, категории, числовое равенство, строгие consumers, map keys и CLI. Сохранение имён сценариев проверено относительно текущей main spec; baseline nil/Atom записан в design.md. Строгая OpenSpec-валидация прошла.
- Независимое read-only review: замечаний нет. Аудит отрицательных индексов, нулевых делителей и callbacks на пустых коллекциях относится к следующему change; non-finite/overflow, произвольная точность и расширение quote остаются заявленными Non-Goals.
