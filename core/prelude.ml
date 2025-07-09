(* THIS FILE IS GENERATED *)

let prelude_java_macro = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(defn list [& xs] xs)

(defn macro_conj [xs x]
  (list 'y2k.RT.conj xs x))

(defn macro_map [f xs]
  (list 'y2k.RT.map f xs))

(defn macro_eprintln [& xs]
  (concat (list 'y2k.RT.eprintln) xs))

(defn macro_FIXME [& xs]
  (list 'java.util.Objects.requireNonNull
        nil
        (concat
         (list 'str)
         xs)))

(defn macro_not= [x y]
  (list 'not (list '= x y)))

(defn macro_rest [xs]
  (let [xs_var (gensym)]
    (list
     'do
     (list
      'let* xs_var (list 'cast 'java.util.List xs))
     (list '.subList xs_var 1 (list '.size xs_var)))))

(defn macro_concat [& xs]
  (concat (list 'y2k.RT.concat)
          xs))

(defn macro_merge [m1 m2]
  (list 'y2k.RT.merge m1 m2))

(defn macro_drop [n xs]
  (list 'y2k.RT.drop n xs))

(defn macro_first [xs]
  (list 'get xs 0))

(defn macro_empty? [xs]
  (list '= 0 (list 'count xs)))

(defn macro_some? [x]
  (list 'not (list 'nil? x)))

(defn macro_nil? [x]
  (list '= 'nil x))

(defn macro_into-array2 [class col]
  (list 'y2k.RT.into_array class col))

(defn macro_assoc [xs k v]
  (list 'y2k.RT.assoc xs k v))

(defn macro_atom [x]
  (list 'java.util.concurrent.atomic.AtomicReference. x))

(defn macro_reset! [a x]
  (list
   '.set
   (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
   x))

(defn macro_swap! [a f]
  (list
   '.getAndUpdate
   (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
   (list 'cast 'java.util.function.UnaryOperator f)))

(defn macro_deref [a]
  (list
   '.get
   (list 'cast 'java.util.concurrent.atomic.AtomicReference a)))

(defn macro_comment [x] (list 'do))

(defn macro_defn- [name args & body]
  (concat (list 'defn name args) body))

(defn macro_= [x y]
  (list 'java.util.Objects.equals x y))

(defn macro_hash-map [& xs]
  (concat
   (list 'java.util.Map.of)
   xs))

(defn macro_count [xs]
  (let* vxs (gensym))
  (list
   'do
   (list 'let vxs xs)
   (list
    'if
    (list 'instance? 'java.util.Map vxs)
    (list '. (list 'cast 'java.util.Map vxs) 'size)
    (list '. (list 'cast 'java.util.Collection vxs) 'size))))

(defn macro_get [xs k]
  (list 'y2k.RT.get xs k))

;; (defn macro_get [xs i]
;;   (let* vxs (gensym))
;;   (let* vi (gensym))
;;   (list
;;    'do
;;    (list 'let vxs xs)
;;    (list 'let vi i)
;;    (list 'if
;;          (list 'instance? 'java.util.Map vxs)
;;          (list '. (list 'cast 'java.util.Map vxs) 'get vi)
;;          (list '. (list 'cast 'java.util.List vxs) 'get (list 'cast 'int (list 'cast 'Object vi))))))

(defn macro_str [& xs]
  (concat
   (list
    'String.format
    (reduce (fn* [acc x] (str acc "%s")) "" xs))
   xs))

(defn macro_vector [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))
|}

let prelude_eval_macro = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(defn macro_comment [x]
  (list 'do))
|}

let prelude_eval = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(def* vector
  (fn* [& xs] xs))
|}

let prelude_js_macro = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(defn macro_comment [x]
  (list 'do))

(defn macro_println [& xs]
  (concat (list 'console.log) xs))

(defn macro_str [& xs]
  (concat (list '+ "") xs))

(defn macro_string? [x]
  (list '= (list 'type x) "string"))

(defn macro_nil? [x]
  (list '= x nil))

(defn macro_some? [x]
  (list 'not (list '= x nil)))

(defn macro_FIXME [& xs]
  (list
   (list 'fn
         (vector)
         (list 'throw
               (list 'Error.
                     (concat (list 'str) xs)))
         nil)))

;; Collections

(defn macro_concat [& xs]
  (concat
   (list
    '.concat
    [])
   xs))

(defn macro_empty? [xs]
  (list '= 0 (list '.-length xs)))

(defn macro_count [xs]
  (list '.-length xs))

;; Atoms

(defn macro_reset! [a x]
  (list 'assoc! a 0 x))

(defn macro_atom [x]
  (list 'vector x))

(defn macro_deref [a]
  (list 'get a 0))
|}
