## Why

Опубликованные effect packages используют Clojure-совместимые associative destructuring patterns вида `{url :url}`, тогда как frontend поддерживает только key-first форму `{:url url}`. Текущее дешугорирование превращает reverse-форму в `(hash-map url "url")`, из-за чего eval не создаёт binding `url`, а JavaScript и Java получают некорректные ссылки.

## What Changes

- Нормализовать пары `binding :keyword` внутри brace-patterns макросов `let` и `fn` в существующую canonical key-first форму `:keyword binding` до обычного раскрытия keyword и map.
- Применять нормализацию рекурсивно к вложенным sequential и associative patterns; `defn` и `defn-` получают поведение через существующее раскрытие в `fn`.
- Не менять обычные map-выражения, прямые core-формы `let*`/`fn*`, явно записанные `(hash-map ...)` patterns и существующую key-first форму.
- Проверить поведение одним общим sample на eval, JavaScript и Java и точными frontend-тестами раскрытия `let`/`fn`.

## Capabilities

### New Capabilities

Нет.

### Modified Capabilities

- `frontend-syntax`: associative binding patterns в `let` и `fn` принимают reverse keyword-пары как compatibility alias и нормализуют их в существующее key-first представление.

## Impact

- `frontend/builtin_macros.ml`: общий рекурсивный helper рядом с `let_macro` и `fn_macro`, вызываемый только из этих macros.
- `test/frontend_desugar_test.ml` и один fixture в `test/samples/`: regression coverage для macro expansion и одинакового исполнения на всех targets.
- Eval, compiler lowering, JavaScript/Java generators и runtimes не меняются и продолжают получать canonical `(hash-map key binding ...)` patterns.
