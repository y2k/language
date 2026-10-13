;; (true true true true true true true)|false|false|false|false|false|false|false|false|false|false|true|true|true|no|true|1
(ns checks.string.core)

(defn make-text [calls]
  (swap! calls (fn [n] (+ n 1)))
  "text")

(defn test []
  (let [calls (atom 0)]
    (str (map (fn [value] (string? value)) ["" "text" "nil" "false" "42" :name (str 42)])
         "|" (string? nil)
         "|" (string? true)
         "|" (string? false)
         "|" (string? 42)
         "|" (string? 0.25)
         "|" (string? [])
         "|" (string? {})
         "|" (string? (fn [x] x))
         "|" (string? (atom "text"))
         "|" (string? (re-pattern "text"))
         "|" (string? (get {:text "text"} :text))
         "|" (= (string? "") true)
         "|" (= (string? nil) false)
         "|" (if (string? 42) "yes" "no")
         "|" (string? (make-text calls))
         "|" (deref calls))))
