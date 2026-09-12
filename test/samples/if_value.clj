;; 7|12|3|nil|false|3|ab

(def initial (if true 7 99))

(defn trace-value [events label value]
  (swap! events (fn [old] (str old label)))
  value)

(defn test []
  (let [value (if false 99 (+ 10 (if true 2 99)))
        events (atom "")
        ordered (+ (trace-value events "a" 1)
                   (if (trace-value events "b" true)
                     2
                     (trace-value events "bad" 99)))
        journal (deref events)]
    (str initial "|" value "|"
         (if (if true false true) 99 3) "|"
         (if false 1) "|"
         (if true false true) "|"
         ordered "|" journal)))
