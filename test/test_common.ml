let tests =
  [
    ( __LOC__,
      {|(defn test []
(if      (contains? {:a 1 :b 2} :b)                       nil (FIXME))
(if (not (contains? {:a 1 :b 2} :c))                      nil (FIXME))
(if (vector? [1 2])                                       nil (FIXME))
(if (= 2 (second [1 2]))                                  nil (FIXME))
(if (string? "")                                          nil (FIXME))
(if (boolean? false)                                      nil (FIXME))
(if (= 6 (reduce (fn [^int a ^int b] (+ a b)) 0 [1 2 3])) nil (FIXME))
(if (= (str [1 2]) (str (vec (list 1 2))))                nil (FIXME))
(println  "1" "2")
(eprintln "1" "2")
(let t1 (println  "1" "2"))
(let t2 (eprintln "1" "2"))
  0)|},
      "0" );
  ]
