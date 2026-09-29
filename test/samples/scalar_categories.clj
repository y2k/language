;; false false false true true true

(defn test []
  (str (= 42 "42") " " (= (+ 20 22) "42") " " (= (count [1 2]) "2") " "
       (= :name "name") " " (= (:name {:name 42}) 42) " " (= (str 42) "42")))
