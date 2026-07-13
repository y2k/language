;; X!

(defn test []
  (.orElse
   (.map
    (java.util.Optional/of "X")
    ^java.util.function.Function
    (fn [value] (str value "!")))
   "missing"))
