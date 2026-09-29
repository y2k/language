;; first first text symbol string 5|ok ok ok|true true true true|20

(defn test []
  (let [m (hash-map 1 "first" 1.0 "second" "1" "text" 'x "symbol" "x" "string")
        f (fn [x] x)
        a (atom 1)]
    (str (get m 1) " " (get m 1.0) " " (get m "1") " "
         (get m 'x) " " (get m "x") " " (count m) "|"
         (let [{1 x} (hash-map 1.0 "ok")] x) " "
         ((fn [{1 x}] x) (hash-map 1.0 "ok")) " "
         (get-in (hash-map 1.0 {:x "ok"}) [1 :x]) "|"
         (= (get (hash-map f 1) f) nil) " "
         (= (get (hash-map a 1) a) nil) " "
         (= (get (hash-map + 1) +) nil) " "
         (= (get (hash-map [f] 1) [f]) nil) "|"
         (get [10 20] (+ 0.5 0.5)))))
