;; true|true|true|true|true|true|true|true|true|true|true
(ns checks.effects.core)

(def effects (atom ""))
(def calls (atom 0))

(defn record! [item]
  (swap! effects (fn [text] (str text item)))
  (swap! calls (fn [n] (+ n 1)))
  "ignored")

(defn test []
  (reset! effects "")
  (reset! calls 0)
  (let [result (run! record! [1 2 3])
        ordered (= (deref effects) "123")
        once (= (deref calls) 3)
        empty-result (run! record! [])
        no-empty-calls (= (deref calls) 3)
        lambda-result (run! (fn [item] (record! item)) (list 4 5))
        local record!
        local-result (run! local [6])]
    (str ordered "|" once "|" (= result nil) "|" (not= result "nil") "|"
         (not result) "|" (= empty-result nil) "|" no-empty-calls "|"
         (= lambda-result nil) "|" (= local-result nil) "|"
         (= (deref effects) "123456") "|" (= (deref calls) 6))))
