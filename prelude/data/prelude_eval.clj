(defn not [x] (if x false true))

(defn nil? [x] (= x nil))
(defn some? [x] (not (nil? x)))

(defn not= [a b] (not (= a b)))

(defn first [xs] (get xs 0))
(defn second [xs] (get xs 1))
