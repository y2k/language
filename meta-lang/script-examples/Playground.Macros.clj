;; (->
;;  (a b)
;;  (c d)
;;  (e f))

;; (e (c (a b) d) f)

(defmacro arrow [body]
  (reduce
   (fn [a x] (cons (first x) (cons a (rest x))))
   body))

[(or 1 2 3)
 (or nil 1 2 3)
 (or nil nil)
 (or nil false)
 (or nil true)
 (or false 1)
 (or false nil 2)
 (or nil false 3)
 (or true 1)]
