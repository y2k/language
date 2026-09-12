;; nil|nil|2|false|nil|1|12|1|20|test-a,test-b,result-b

(defn record [journal label value]
  (swap! journal (fn [old] (str old label)))
  value)

(defn test []
  (let [journal (atom "")
        selected (cond
                   (record journal "test-a," false) (record journal "result-a," 10)
                   (record journal "test-b," true) (record journal "result-b" 20)
                   (record journal "test-c," true) (record journal "result-c," 30)
                   :else (record journal "fallback," 40))
        events (deref journal)]
    (str (cond) "|"
         (cond false 1 nil 2) "|"
         (cond false 1 true 2 :else 3) "|"
         (cond true false :else 3) "|"
         (cond true nil :else 3) "|"
         (cond :else 1 true 2) "|"
         (+ 10 (cond false 1 :else 2)) "|"
         (cond 0 1 :else 2) "|"
         selected "|" events)))
