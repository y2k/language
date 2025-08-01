(defn macro_declare [x] (list 'do))

(defn macro_boolean [x]
  (list 'y2k.RT.toBoolean x))

(defn macro_unixtime []
  (list '/ (list '.getTime (list 'java.util.Date.)) 1000.0))

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
  (let [var (gensym)]
;;
    (list 'let (list 'vector var x)
          (list '.set
                (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
                var)
          var)
;;
    ))

;; (defn macro_reset! [a x]
;;   (list
;;    '.set
;;    (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
;;    x))

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

;; Collections

(defn macro_shuffle [seed xs]
  (list 'y2k.RT.shuffle seed xs))

(defn macro_take [n xs]
  (list 'y2k.RT.take n xs))
