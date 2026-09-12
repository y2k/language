;; zero|a|ab|abc|abcd|callback|get|if|local|inline|once|1

(defn make-zero [] (fn [] "zero"))
(defn make-one [] (fn [a] a))
(defn make-two [] (fn [a b] (str a b)))
(defn make-three [] (fn [a b c] (str a b c)))
(defn make-four [] (fn [a b c d] (str a b c d)))

(defn callback [onclick]
  ((onclick) "callback"))

(defn counted-factory [calls]
  (swap! calls (fn [n] (+ n 1)))
  (fn [value] value))

(defn test []
  (let [calls (atom 0)
        result ((counted-factory calls) "once")
        local (make-one)]
    (str ((make-zero)) "|"
         ((make-one) "a") "|"
         ((make-two) "a" "b") "|"
         ((make-three) "a" "b" "c") "|"
         ((make-four) "a" "b" "c" "d") "|"
         (callback (fn [] (fn [value] value))) "|"
         ((get [(fn [value] value)] 0) "get") "|"
         ((if true (fn [value] value) (fn [value] nil)) "if") "|"
         (local "local") "|"
         ((fn [value] value) "inline") "|"
         result "|" (deref calls))))
