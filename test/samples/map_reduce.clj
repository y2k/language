;; a1b2

(defn test []
  (reduce
   (fn [acc [k v]] (str acc k v))
   ""
   {:a 1 :b 2}))
