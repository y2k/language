;; true true true false false|true false true false true|true true true true true true

(defn pass [x]
  (let [cell (atom x)
        [a] [(deref cell)]
        {:value b} {:value a}]
    (reset! cell b)
    (swap! cell (fn [old] old))))

(defn test []
  (str (= '42 42) " " (= '0.25 0.25) " " (= 'foo 'foo) " "
       (= 'foo "foo") " " (= (get '(42 "42" foo) 1) 42) "|"
       (= 1 1.0) " " (not= 1 1.0) " " (= 0 -0.0) " "
       (= [1] ["1"]) " " (= {:x 1} {:x 1.0}) "|"
       (= (pass 42) 42) " " (= (pass "42") "42") " "
       (= (pass 'foo) 'foo) " " (= (pass "foo") "foo") " "
       (= (pass false) false) " " (= (pass nil) nil)))
