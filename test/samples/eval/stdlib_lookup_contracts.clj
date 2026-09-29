;; first first first first text 3|b s b s b s|symbol string symbol string|true true true true

(defn test []
  (let [m (hash-map 1 "first" 1.0 "second" "1" "text")
        b (hash-map false "b" "false" "s")
        symbols (hash-map 'x "symbol" "x" "string")
        f (fn [x] x)
        a (atom 1)]
    (str (get m 1.0) " " (get-in m [1]) " "
         (let [{1.0 x} m] x) " " ((fn [{1 x}] x) m) " " (get m "1") " " (count m) "|"
         (get b false) " " (get-in b ["false"]) " "
         (let [{false x "false" y} b] (str x " " y)) " "
         ((fn [{false x "false" y}] (str x " " y)) b) "|"
         (get symbols 'x) " " (get-in symbols ["x"]) " "
         (let [{x a "x" b} symbols] (str a " " b)) "|"
         (= (get (hash-map f 1) f) nil) " "
         (= (get-in (hash-map a 1) [a]) nil) " "
         (= (get (hash-map + 1) +) nil) " "
         (= (get-in (hash-map [f] 1) [[f]]) nil))))
