## Why

В Android-коде проверка контейнера ViewGroup сейчас требует `Class/forName` и строкового имени класса. Issue #20 предлагает короткую статическую проверку `(instance? ViewGroup child)`, использующую обычные Java imports и обнаруживающую неизвестный класс при компиляции.

## What Changes

- Добавить Java-only форму `(instance? TYPE value)` для статически названного класса или интерфейса.
- Генерировать нативный `instanceof`, возвращающий boolean для объектов, подклассов, реализаций интерфейсов, `nil` и посторонних значений; вычислять `value` ровно один раз, включая позицию с отброшенным результатом.
- Диагностировать неверную арность и недопустимый операнд типа; проверку существования и доступности класса оставить `javac`.
- Явно отклонять форму на eval и JavaScript и описать ограничение в справочнике языка.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `compiler-targets`: контракт Java `instance?`, диагностика формы и явное отсутствие поддержки на остальных targets.

## Impact

Изменение затрагивает `backend_compiler/java.ml`, общий lowering, диагностику в `backend_compiler/js.ml` и `backend_eval/eval.ml`, целевые тесты и `skills/y2k-language/SKILL.md`. Для проверки нужны генерация Java, `javac` и исполнение на JDK; Android SDK и новые зависимости не нужны. Runtime lookup, новый resolver классов и изменения runtime-библиотек не требуются.
