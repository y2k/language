;; (keep)

(defn test []
  (let [xs (java.util.ArrayList. (list "drop" "keep"))
        _ (.removeIf xs
                     ^java.util.function.Predicate
                     (fn [value] (.equals value "drop")))]
    xs))
