;; true|true|true|true|true
(defn ids [items]
  (reduce (fn [text item] (str text (:id item))) "" items))

(defn test []
  (let [numbers [{:id "A" :key 10} {:id "B" :key -2} {:id "C" :key 1.5}
                 {:id "D" :key 1} {:id "E" :key 0.25} {:id "F" :key -2.5}]
        ties [{:id "A" :key 1} {:id "B" :key 1.0} {:id "C" :key 0}
              {:id "D" :key -0.0} {:id "E" :key 0.0}]
        strings [{:id "A" :key "a"} {:id "B" :key "2"} {:id "C" :key "A"}
                 {:id "D" :key "10"} {:id "E" :key ""} {:id "F" :key "aa"}
                 {:id "G" :key "a"}]
        sorted-numbers (sort-by (fn [item] (:key item)) numbers)
        sorted-ties (sort-by (fn [item] (:key item)) ties)
        sorted-strings (sort-by (fn [item] (:key item)) strings)
        values (sort-by (fn [item] 0) [nil false "nil"])]
    (str (= (ids sorted-numbers) "FBEDCA") "|"
         (= (ids sorted-ties) "CDEAB") "|"
         (= (ids sorted-strings) "EDBCAGF") "|"
         (and (= (ids numbers) "ABCDEF") (= (ids ties) "ABCDE") (= (ids strings) "ABCDEFG")) "|"
         (and (= (get values 0) nil) (= (get values 1) false)
              (= (get values 2) "nil") (not= (get values 2) nil)))))
