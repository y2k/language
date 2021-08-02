(module
 ; int -> int -> int
 (defn mul [a b]
   (sys/invoke-pure "runtime/int_mul" a b))
 ; int -> int -> int
 (defn add [a b]
   (sys/invoke-pure "runtime/int_add" a b))
 ; string -> int
 (defn from-string [a]
   (sys/invoke-pure "runtime/int_make" a))
 ; sexp -> int
 (defn make [a]
   (sys/invoke-pure "runtime/int_make" a)))

(module
 ; float -> float -> float
 (defn add [a b]
   (sys/invoke-pure "runtime/float_add" a b))
 ; string -> float
 (defn from-string [a]
   (sys/invoke-pure "runtime/float_make" a))
 ; sexp -> float
 (defn make [a]
   (sys/invoke-pure "runtime/float_make" a)))

(module
 ["int" :as int]
 ["float" :as float]

 (defn main []
   (float/add
    (float/make '2.0)
    (float/make "2.0"))
   (int/add
    (int/make '2)
    (int/make '2))))

(module
 ; Prelude
 ["int" :as int]
 ["float" :as float]

 (defn + [a b]
   (int/add a b))
 (defn +. [a b]
   (float/add a b)))

(module
 ["int" :as int]
 ["float" :as float]

 (defn main []
   (+ 2 2)
   (+. 2.0 2.0))
 (defn main []
   (int/add 2 2)
   (float/add 2.0 2.0))
 (defn main []
   (int/add 2 2)
   (float/add 2.0 2.0)))