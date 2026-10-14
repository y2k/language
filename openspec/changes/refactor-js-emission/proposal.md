# Proposal

## Why

В `inline-js-source-maps` смешаны изменение внутренней сборки JS и добавление карт. Выносим композицию отложенных фрагментов в отдельный предварительный change, чтобы проверить сохранение генерации до добавления mapping.

## What Changes

- Внутренняя сборка `backend_compiler/js.ml` переходит с конкатенации строк на отложенные фрагменты, записываемые в один буфер.
- Публичный результат `compile` остаётся строкой; generated JS должен совпадать с исходным генератором побайтово.
- Рефакторинг принимается самостоятельно и становится prerequisite для `inline-js-source-maps`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

Нет: поведение и требования не меняются. `.openspec.yaml` задаёт `skip_specs: true`.

## Impact

`backend_compiler/js.ml` и проверки совместимости JS-вывода. CLI, runner, AST, lowering, runtime и Java generator сохраняют существующие контракты. Source maps, координаты, origin и новые параметры CLI относятся к последующему change.
