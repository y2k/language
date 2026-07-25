;; first:a,second:b

(defn call-fn [f value]
  (f value))

(defn call-shadowed [str value]
  (str value))

(defn test []
  (str (call-fn (fn [value] (str "first:" value)) "a")
       ","
       (call-shadowed (fn [value] (str "second:" value)) "b")))
