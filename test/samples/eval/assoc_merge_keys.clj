;; true true true true true true true true true

(defn test []
  (let [m (hash-map 1 "first" 1.0 "second" "1" "text")
        a (assoc m 1.0 "new")
        b (assoc a 1 "again")
        c (merge m (hash-map 1.0 "right"))]
    (str (= (get a 1) "new") " "
         (= (get a 1.0) "new") " "
         (= (get a "1") "text") " "
         (= (count a) 2) " "
         (= (count b) 2) " "
         (= (get b 1.0) "again") " "
         (= (get c 1) "right") " "
         (= (count m) 3) " "
         (= (get m 1) "first"))))
