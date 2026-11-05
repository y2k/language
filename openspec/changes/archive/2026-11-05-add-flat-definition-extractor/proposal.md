# Proposal

## Why

Текстовый FEX bundle нужно материализовать в отдельные файлы RHS для доставки и загрузки runtime. Extractor (unpacker) выделяется из compiler change, чтобы независимо реализовать файловый этап по готовому транспортному контракту.

## What Changes

- Добавить `--unpack-fex --output-dir PATH`: bundle из stdin, отдельные файлы RHS и список обработанных keys в stdout.
- Реализовать последовательное декодирование Base64 в отдельном Dune-модуле без разбора FEX.
- Сохранять mtime при совпадении bytes, перезаписывать изменённые и повторные keys, оставлять соседние файлы.
- Проверять framing, Base64 и безопасность filename при обработке каждой пары.

## Capabilities

### New Capabilities

- `flat-definition-extractor`: распаковка FEX bundle в именованные файлы RHS.

### Modified Capabilities

Нет. Формат bundle, FEX и canonical keys определяет `compiler-targets` в `add-flat-definition-target`.

## Impact

Новая Dune-библиотека `fex_unpack/`, подключение через `bin/dune` и `bin/main.ml`, CLI fixtures и английский справочник `skills/y2k-language/`. Зависимость — контракт и compiler из `add-flat-definition-target`; исполнение файлов относится к `add-flat-definition-interpreter`. Изменения make и доставка остаются отдельными задачами; новые внешние runtime-зависимости не предполагаются.
