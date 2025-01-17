;; Shared definitions for all targets

(def false 0)
(def hash-map 0)
(def true 0)
(def vector 0)

(defmacro comment [& args] 'nil)
(defmacro first [xs] (list 'get xs 0))
(defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude
