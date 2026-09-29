;; yes 1 1|no 1 1|nil 1 0

(defn choose [value]
  (let [conditions (atom 0)
        branches (atom 0)
        result (if (do (swap! conditions (fn [n] (+ n 1))) value)
                 (do (swap! branches (fn [n] (+ n 1))) "yes")
                 (do (swap! branches (fn [n] (+ n 1))) "no"))]
    (str result " " (deref conditions) " " (deref branches))))

(defn test []
  (let [conditions (atom 0)
        branches (atom 0)
        result (if (do (swap! conditions (fn [n] (+ n 1))) false)
                 (swap! branches (fn [n] (+ n 1))))]
    (str (choose "false") "|" (choose false) "|"
         result " " (deref conditions) " " (deref branches))))
