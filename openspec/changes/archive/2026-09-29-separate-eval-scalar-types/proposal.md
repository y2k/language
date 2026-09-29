# Proposal

## Why

После выделения boolean eval всё ещё смешивает строки, числа и цитированные символы в `Symbol`. Это делает `42` равным `"42"` и не позволяет встроенным функциям проверять строковые и числовые аргументы по runtime-типу.

## What Changes

- Разделить runtime-строки, целые числа, binary64-числа и символы; сохранить `Nil` и boolean предыдущего change.
- **BREAKING**: строки, числа и символы разных категорий не равны; `'foo` отличается от `"foo"`; цитированные числа остаются числами.
- Сравнивать числа по значению: `(= 1 1.0)` возвращает `true`. Применить это равенство также к коллекциям, ключам map и ассоциативной деструктуризации.
- **BREAKING**: функции и Atom в позиции ключа не находятся, в соответствии с их существующим неравенством через `=`; host structural comparison больше не используется для lookup.
- Сохранить keywords как строки, существующий lookup namespaces и контракт дробной арифметики, включая нормализацию целых результатов.
- Перевести производителей и потребителей скаляров на новые типы: арифметику, `count`, индексы, `str`, `slurp`, `deps`, диагностику и runner.
- **BREAKING**: числовые строки и символы перестают автоматически приниматься вместо чисел; числовые пути `slurp` и версии `deps` перестают приниматься вместо строк.
- Обновить справочник и тесты; полный аудит арностей и ошибок выполняется следующим change.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: типизированные скаляры, числовое равенство, цитирование, lookup ключей и адаптация stdlib/вывода.

## Impact

- Зависит от реализации `add-eval-booleans`; deltas описывают состояние после неё.
- Реализация: `backend_eval/eval_types.ml`, `backend_eval/eval.ml`, `backend_eval/eval_stdlib.ml`, `bin/runner.ml`.
- Проверки: eval OCaml suites, включая helpers в `test/eval_ns_test.ml`, `test/eval_deps_test.ml`, `test/atoms_test.ml`, общие и eval-only samples; справочник `skills/y2k-language/SKILL.md`.
- `frontend/builtin_macros.ml` уже преобразует keywords в строки: отдельный keyword-тип и новая семантика macro expansion не требуются.
- Общий контракт `fractional-arithmetic` сохраняется. JS/Java, Re, regex, произвольная точность чисел и дробные `/`/операции порядка вне scope.
- Следующий change: `enforce-eval-stdlib-contracts`.
