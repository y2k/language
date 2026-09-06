## Context

См. `proposal.md` для мотивации и delta specs для требуемого поведения. Frontend уже munges символ `not=` в `not_EQ_`, а все generators передают его по generic function-call path. JavaScript получает runtime-функции через явный unconditional import, Java использует wildcard static import, evaluator разрешает core names через stdlib.

Существующая equality различается за пределами принятого scalar-контракта: evaluator поддерживает variadic structural `=`, Java `_EQ_` использует `Objects.equals`, JavaScript `_EQ_` использует `Object.is`. Файлы `prelude/language_runtime.js` и `prelude/language_runtime.java` являются links на versioned package в соседнем репозитории `packages`.

## Goals / Non-Goals

**Goals:**

- Сохранить generic function-call compilation и существующее symbol munging.
- Реализовать `not=` через существующую equality semantics каждого target без дублирования comparison logic.
- Закрепить общий двухаргументный scalar-контракт одним cross-target execution sample.

**Non-Goals:**

- Вводить compiler special form, frontend macro, builtin registry или selective JavaScript imports.
- Расширять или унифицировать variadic, structural, mixed-type и wrong-arity behavior существующего `=`.
- Определять compile-time validation неизвестных function names.

## Decisions

- Реализовать `not=` как обычную runtime-функцию: binding `not=` в evaluator и munged helper `not_EQ_` в JavaScript и Java runtimes. Compiler special case и frontend macro отклонены, потому что они создали бы отдельный путь и могли бы перехватить lexical binding с именем `not=`.
- Каждый target вычисляет `not=` как boolean negation результата своей существующей equality operation. Evaluator переиспользует `equal`, JavaScript helper вызывает `_EQ_`, Java helper вызывает `_EQ_`; отдельная comparison abstraction не добавляется.
- Добавить `not_EQ_` в существующий unconditional JavaScript runtime import. Анализ используемых builtins и selective import отклонены как несоразмерное архитектурное расширение. Java generator менять не требуется благодаря wildcard static import.
- Добавить один common sample с равными и различными scalar-значениями. Существующий harness исполняет каждый common sample через eval, Node и Java, поэтому он одновременно покрывает runtime export/import, Java compilation и cross-target result; отдельный JS execution harness не нужен.
- Обновить snapshots полного JavaScript runtime import после добавления identifier. Это сохраняет текущий snapshot contract вместо несвязанной переработки import generation.

## Risks / Trade-offs

- [Generated JavaScript потребует runtime, экспортирующий `not_EQ_`] -> Обновить linked versioned runtime до изменения compiler import и поставлять их согласованно.
- [Поведение `not=` расходится между targets для collections или нестандартной arity вслед за `=`] -> Не заявлять эти случаи в контракте и не включать их в regression sample.
- [Evaluator фактически может унаследовать более широкую semantics variadic `=`] -> Считать это негарантированным поведением; не добавлять отдельную валидацию только ради ограничения реализации.
- [Unconditional import добавляет ещё один identifier во все generated modules] -> Следовать существующей простой runtime architecture; selective imports остаются вне scope.

## Migration Plan

1. Добавить `not_EQ_` в JavaScript и Java versioned runtimes, на которые указывают `prelude/language_runtime.*`.
2. Добавить evaluator binding и JavaScript runtime import в репозитории `language`.
3. Добавить common sample, обновить snapshots и выполнить `make test` с согласованными runtime-файлами.

При rollback сначала удалить `not_EQ_` из JavaScript compiler import и evaluator stdlib. Additive runtime helpers можно оставить либо удалить после отката compiler dependency.
