;; true false true false|false false|true true true true|true true 2 true true false|false (false true) {enabled false}

(defn test []
  (let [cell (atom false)
        initial (deref cell)
        reset-value (reset! cell true)
        swapped (swap! cell (fn [old] (not old)))
        [[nested] {"enabled" enabled}] [[false] {"enabled" false}]]
    (str (= 'false false) " " (= 'false "false") " "
         (= (get '(false true "false") 1) true) " "
         (= (get '(false true "false") 2) false) "|"
         (= [false] ["false"]) " " (= {:x false} {:x "false"}) "|"
         (= initial false) " " (= reset-value true) " " (= swapped false) " "
         (= (deref cell) false) "|"
         (= ((fn [x] x) false) false) " "
         (= (get-in {:x [false]} [:x 0]) false) " "
         (if nested 1 2) " " (= enabled false) " "
         (= (assert "false") true) " " (not (str false)) "|"
         false " " [false true] " " {"enabled" false})))
