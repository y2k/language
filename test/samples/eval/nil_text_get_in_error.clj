;; get expects a hash-map/list and a key/index

(defn test []
  (get-in {:value "nil"} [:value :missing]))
