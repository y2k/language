# Статически типизированный встраиваемый LISP с интеропом

### Примеры использования

- https://github.com/y2k/relax_cats_bot
- https://github.com/y2k/declarative_ban_bot
- https://github.com/y2k/charge_timer (с компиляцией в Java для Android)
- https://github.com/y2k/declarative_notify

### Платформы компиляции

- JS
- Java

### Пример кода

```clojure
(ns app
 (:import [android.widget Toast]
 	        [android.content Context])

(defn- show [^Context context ^String text]
  (.show
    (Toast/makeText context text Toast.LENGTH_LONG)))

(defn main [^Context context]
  (show context "Hello World")))
```
