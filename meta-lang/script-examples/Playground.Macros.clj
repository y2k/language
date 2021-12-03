;; (->
;;  (a b)
;;  (c d)
;;  (e f))

;; (e (c (a b) d) f)

(defmacro arrow [body]
  (reduce
   (fn [a x] (cons (first x) (cons a (rest x))))
   body))
