;; true true true true true true

(ns checks.maps.core)

(defn test []
  (let [left {:a 1 :b 2 :nested {:old 1}}
        right {:a 3 :c 4 :nested {:new 2}}
        added (assoc left :c 5)
        replaced (assoc left :a 6)
        merged (merge left right)
        cell (atom 1)
        shared (merge {} (assoc {} :cell cell))
        item {:reason "selected" :status "pending"}
        event (assoc (merge item {:status "ready"}) :selection_reason (:reason item))]
    (reset! (:cell shared) 7)
    (str (and (= (:c added) 5) (= (:a added) 1) (= (count added) 4)
              (= (:a replaced) 6) (= (count replaced) 3)) " "
         (and (= (:a merged) 3) (= (:b merged) 2) (= (:c merged) 4)
              (= (count merged) 4) (= (get-in merged [:nested :new]) 2)
              (= (count (:nested merged)) 1)) " "
         (and (= (:a left) 1) (= (count left) 3) (= (get-in left [:nested :old]) 1)
              (= (:a right) 3) (= (count right) 3)) " "
         (and (= (count (merge {} {})) 0) (= (:a (merge {} {:a 1})) 1)
              (= (:a (merge {:a 1} {})) 1)) " "
         (= (deref cell) 7) " "
         (and (= (:selection_reason event) "selected") (= (:status event) "ready")
              (= (:status item) "pending")))))
