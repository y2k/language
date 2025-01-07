;; Shared definitions for all targets

(def false 0)
(def hash-map 0)
(def true 0)
(def vector 0)

(defmacro not= [a b] (list 'not (list '= a b)))
(defmacro comment [& args] 'nil)

;; Specific target prelude
