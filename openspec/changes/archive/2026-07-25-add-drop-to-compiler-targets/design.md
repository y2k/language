## Context

Evaluator уже предоставляет `drop` для списков. JavaScript generator импортирует runtime-функции явным списком, а Java generator использует wildcard static import. `prelude/language_runtime.js` и `prelude/language_runtime.java` являются symlink на versioned `prelude/1.0.0` в отдельном packages repo, поэтому runtime-изменения версионируются там.

## Goals / Non-Goals

**Goals:**

- Обеспечить одинаковое наблюдаемое поведение `drop` на eval, JavaScript и Java.
- Проверить поведение существующим кросс-бэкендным sample harness.

**Non-Goals:**

- Расширять `drop` на типы, отличные от list.
- Менять существующую реализацию evaluator или задавать поведение для неверных аргументов.

## Decisions

- Использовать стандартные операции целевых платформ: `slice` в JavaScript и копию ограниченного `subList` в Java. Это минимально и сохраняет порядок элементов.
- Добавить один общий sample, который автоматически выполняется на всех трёх targets, вместо отдельных backend-тестов.
- Добавить `drop` в явный JavaScript runtime import; Java не требует изменения generator благодаря существующему wildcard static import.

## Risks / Trade-offs

- Различия native-типов чисел при неверном аргументе не покрываются контрактом → sample проверяет только целые значения, включая граничные случаи.
- Java `subList` является представлением исходного списка → runtime возвращает копию, чтобы результат не зависел от последующих структурных изменений исходного списка.
