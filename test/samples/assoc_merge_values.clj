;; true true true true true true true true

(defn test []
  (let [old {:x 42}
        a (assoc old :x nil)
        b (assoc old :x false)
        c (assoc old :x 0)
        d (assoc old :x "")
        left {:a 1 :b 2 :c 3 :d 4}
        right {:a nil :b false :c 0 :d ""}
        merged (merge left right)]
    (str (and (= (count a) 1) (= (:x a) nil) (not= (:x a) "nil") (not (:x a))) " "
         (and (= (count b) 1) (= (:x b) false) (not= (:x b) "false") (not (:x b))) " "
         (and (= (:x c) 0) (if (:x c) true false)) " "
         (and (= (:x d) "") (if (:x d) true false)) " "
         (and (= (count merged) 4) (= (:a merged) nil) (= (:b merged) false)
              (= (:c merged) 0) (= (:d merged) "")) " "
         (and (= (:x old) 42) (= (count old) 1)) " "
         (and (= (:a left) 1) (= (:b left) 2) (= (:c left) 3) (= (:d left) 4)) " "
         (and (= (count right) 4) (= (:a right) nil) (= (:b right) false)))))
