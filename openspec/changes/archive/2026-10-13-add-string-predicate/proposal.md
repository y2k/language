# Proposal

## Why

В issue #33 приложение events вынуждено проверять строки из JSON через повторяющийся JS interop. Переносимый `string?` позволит проверять значения перед строковыми операциями одинаковым вызовом на eval, JS и Java.

## What Changes

- Добавить доступный без импорта unary-предикат `(string? value)`, возвращающий boolean по фактическому строковому типу runtime.
- Считать пустую строку и keywords строками; nil, boolean, числа, коллекции, функции, Atom и regex — не строками.
- Зафиксировать различия представления цитированных атомов: `'foo` не строка в eval, но строка в JS/Java. JS String-объекты и Java StringBuilder не считаются строками.
- Описать контракт в английском справочнике и проверить общие и target-specific сценарии.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `eval-runtime`: добавить контракт предиката строкового runtime-типа и ошибки неверной арности в eval.
- `compiler-targets`: добавить строковый предикат для JS/Java и документированные границы host-типов и цитирования.

## Impact

- `backend_eval/eval_stdlib.ml`: builtin и регистрация имени.
- `prelude/language_runtime.js`, `prelude/language_runtime.java`: runtime-функции; `backend_compiler/js.ml`: импорт нового binding.
- `test/samples/`, целевые sample-подкаталоги и существующие OCaml-тесты eval: проверка контракта.
- `skills/y2k-language/SKILL.md`: API и различия targets.
- Новых зависимостей и runtime-типов нет. JS-код с новым импортом требует соответствующего обновлённого prelude; публикация пакета не входит в эту правку.
