# Статически типизированный встраиваемый LISP с интеропом

### Примеры использования

- https://github.com/y2k/relax_cats_bot
- https://github.com/y2k/declarative_ban_bot
- https://github.com/y2k/charge_timer (с компиляцией в Kotlin для Android)
- https://github.com/y2k/declarative_notify

### Платформы компиляции

- JS
- Kotlin

### Пример кода

```clojure
(ns app
 (:import [android.widget Toast])

(defn- show [env text]
  (.show 
    (Toast/makeText env.context text Toast.LENGTH_LONG)))

(defn main [env]
  (show env "Hello World")))
```
