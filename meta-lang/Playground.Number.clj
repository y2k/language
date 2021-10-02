;; Foreign functions
; #Sexp -> #Int
(defn int_of_sexp [x] ???)
; #Int -> #Int -> #Int
(defn + [a b] ???)
; #Sexp -> #Float
(defn float_of_sexp [x] ???)
; #Float -> #Float -> #Float
(defn +. [a b] ???)

;; Example
(module
 (defn main []
   (+ 2 2)
   (+. 2.0 2.0)))
