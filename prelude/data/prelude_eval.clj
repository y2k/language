(def* vector (fn* [& xs] xs))

(defn not [x] (if x false true))

(defn nil? [x] (= x nil))
(defn some? [x] (not (nil? x)))
