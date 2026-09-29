# Proposal

## Why

Issue [#24](https://github.com/y2k/language/issues/24) требует собирать standalone Tampermonkey userscript через eval из JS, runtime и заголовка: чтение и конкатенация уже есть, но отсутствуют поиск и замена по regex. После интеграции boolean, scalar types и stdlib contracts можно добавить API с отдельным типом regex и предсказуемыми ошибками.

## What Changes

- Добавить eval-only `re-pattern`, `re-find` и `re-replace` без импорта: компиляция строкового шаблона, первое полное совпадение либо nil, глобальная буквальная замена.
- Использовать согласованную библиотеку Re и диалект `Re.Perl` с настройками по умолчанию, явно описав ограничения относительно Perl/Java Pattern.
- Добавить непрозрачное first-class regex-значение; описать его передачу, истинность, равенство, диагностику, `str` и финальный вывод runner.
- Обеспечить Eval_error для неверных арностей/типов, ошибочных и неподдерживаемых шаблонов; закрепить пустые совпадения и буквальную замену.
- Обновить английский справочник, матрицу контрактов и eval-only тесты, включая сборку небольшого userscript через существующие slurp/str и stdout.

## Capabilities

### New Capabilities

Нет: расширяется существующий eval-runtime.

### Modified Capabilities

- `eval-runtime`: дополнительные bindings regex API и правила нового непрозрачного runtime-значения. Delta добавляет требования, не заменяя существующие scalar/boolean/stdlib контракты.

## Impact

- `backend_eval/eval_types.ml`, `eval_stdlib.ml`, диагностика в `eval.ml`; проверка поведения `bin/runner.ml` и CLI.
- Зависимость `re` в `dune-project`, генерируемом `language.opam` и `backend_eval/dune`.
- `test/eval_stdlib_contracts_test.ml`, новый целевой regex suite с регистрацией в `test/dune`, fixtures в `test/samples/eval/`, `skills/y2k-language/SKILL.md`.
- JS/Java runtime, reader syntax `#"..."`, capture substitution, callback replacement, дополнительные flags-аргументы и полноценный bundler не входят в change.
