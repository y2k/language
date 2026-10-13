;; true|true|true|true|true|true
(ns checks.sorting.core)

(def effects (atom ""))

(defn record-key [item]
  (swap! effects (fn [text] (str text (:id item))))
  (:key item))

(defn ids [items]
  (reduce (fn [text item] (str text (:id item))) "" items))

(defn test []
  (reset! effects "")
  (let [items [{:id "A" :key 20} {:id "B" :key 3} {:id "C" :key 20}]
        named (sort-by record-key items)
        named-calls (= (deref effects) "ABC")
        local record-key
        bound (sort-by local items)
        lambda (sort-by (fn [item] (record-key item)) items)
        empty (sort-by record-key [])
        empty-list (sort-by record-key (list))
        before-single (= (deref effects) "ABCABCABC")
        single (sort-by record-key [{:id "D" :key 1}])]
    (str (and named-calls (= (ids named) "BAC")) "|"
         (and (= (ids bound) "BAC") (= (ids lambda) "BAC")) "|"
         (and (vector? empty) (= (count empty) 0) (not= empty nil)
              (vector? empty-list) (= (count empty-list) 0)) "|"
         (and before-single (= (deref effects) "ABCABCABCD") (= (ids single) "D")) "|"
         (= (ids items) "ABC") "|"
         (= (get (sort-by (fn [x] x) (list 3 1 2)) 0) 1))))
