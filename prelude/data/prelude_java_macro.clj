(defn list [& xs] xs)

;; into-array2

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

(defn macro_get [xs i]
  (let* vxs (gensym))
  (let* vi (gensym))
  (list
   'do
   (list 'let vxs xs)
   (list 'let vi i)
   (list 'if
         (list 'instance? 'java.util.Map vxs)
         (list '. (list 'cast 'java.util.Map vxs) 'get vi)
         (list '. (list 'cast 'java.util.List vxs) 'get (list 'cast 'int (list 'cast 'Object vi))))))

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