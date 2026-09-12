;; 10 20 20 23 23 23 10 1 nil nil false false nil false 7 8 0 9 1 #<atom>

(defn replace-value [reference value]
  (reset! reference value))

(defn test []
  (let [cell (atom 10)
        alias cell
        independent (atom 10)
        calls (atom 0)
        initial (deref cell)
        replaced (replace-value alias 20)
        after-reset (deref cell)
        delta 3
        swapped (swap! cell (fn [old]
                              (swap! calls (fn [n] (+ n 1)))
                              (+ old delta)))
        after-swap (deref cell)
        through-alias (deref alias)
        other-value (deref independent)
        call-count (deref calls)
        reset-nil (reset! cell nil)
        after-nil (deref cell)
        reset-false (reset! cell false)
        after-false (deref cell)
        stored-nil (deref (atom nil))
        stored-false (deref (atom false))
        stored-list (get (deref (atom [7])) 0)
        stored-map (get (deref (atom {:value 8})) :value)
        function-calls (atom 0)
        function-cell (atom (fn [x]
                             (swap! function-calls (fn [n] (+ n 1)))
                             (+ x 1)))
        f (deref function-cell)
        calls-before (deref function-calls)
        function-result (f 8)
        calls-after (deref function-calls)
        self (atom nil)
        self-result (reset! self self)]
    (str initial " " replaced " " after-reset " " swapped " " after-swap " "
         through-alias " " other-value " " call-count " " reset-nil " " after-nil " "
         reset-false " " after-false " " stored-nil " " stored-false " "
         stored-list " " stored-map " " calls-before " " function-result " " calls-after " "
         self-result)))
