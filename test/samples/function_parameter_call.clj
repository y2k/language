;; first:a,second:b,third:c

(defn call-fn [f value]
  (f value))

(defn call-shadowed [str value]
  (str value))

(defn call-fn-map [{:f f} value]
  (f value))

(defn test []
  (str (call-fn (fn [value] (str "first:" value)) "a")
       ","
       (call-shadowed (fn [value] (str "second:" value)) "b")
       ","
       (call-fn-map {:f (fn [value] (str "third:" value))} "c")))
