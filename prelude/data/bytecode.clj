(def - 0)
(def * 0)
(def / 0)
(def + 0)
(def = 0)
(def atom 0)
(def concat 0)
(def deref 0)
(def get 0)
(def if 0)
(def list 0)
(def ns 0)
(def println 0)
(def reset! 0)
(def str 0)
(def vec 0)

(defmacro defonce [name value]
  (list 'if
        (list 'def name)
        'nil
        (list 'def name value)))

(defn not [x] (if x false true))
