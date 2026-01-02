(defn macro_assert [a b]
  (list '= a b))

(defn macro_parse-int [s]
  (list 'Integer/parseInt
        (list 'cast 'String s)))

(defn macro_subs [s sp ep]
  (list '.substring
        (list 'cast 'String s)
        (list 'cast 'int sp)
        (list 'cast 'int ep)))

(defn macro_string/join [sep xs]
  (list 'String/join
        (list 'cast 'String sep)
        (list 'cast 'java.util.Collection xs)))

(defn macro_string/split [s sep]
  (list 'vec
        (list '.split
              (list 'cast 'String s)
              (list 'cast 'String sep))))

(defn macro_string/starts-with? [s prefix]
  (list '.startsWith
        (list 'cast 'String s)
        (list 'cast 'String prefix)))

(defn macro_clojure.string/ends-with? [s suffix]
  (list '.endsWith
        (list 'cast 'String s)
        (list 'cast 'String suffix)))

(defn macro_clojure.string/replace [s match replacement]
  (list '.replace
        (list 'cast 'String s)
        (list 'cast 'String match)
        (list 'cast 'String replacement)))

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

(defn macro_filter [f xs]
  (list 'y2k.RT.filter f xs))

(defn macro_hash-map-from [xs]
  (list 'y2k.RT.hash_map_from xs))

(defn macro_eprintln [& xs]
  (concat (list 'y2k.RT.eprintln) xs))

(defn macro_inc [x]
  (list 'y2k.RT.invoke
        'y2k.prelude_java.inc
        x))

(defn macro_FIXME [& xs]
  (list 'y2k.RT.invoke
        'y2k.prelude_java.fixme
        '__LOC__
        (concat (list 'vector) xs)))

;; (defn macro_FIXME [& xs]
;;   (list 'java.util.Objects.requireNonNull
;;         nil
;;         (list 'string/join
;;               " "
;;               (concat (list 'vector) xs))))

(defn macro_not= [x y]
  (list 'not (list '= x y)))

(defn macro_rest [xs]
  (let [xs_var (gensym)]
    (list
     'do
     (list
      'let xs_var (list 'cast 'java.util.List xs))
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

(defn macro_last [xs]
  (list 'get xs (list '- (list 'cast 'int (list 'count xs)) 1)))

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
    (list 'let (list 'vector var x)
          (list '.set
                (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
                var)
          var)))

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
   (list 'y2k.RT.hash_map)
   xs))

(defn macro_count [xs]
  (let* vxs (gensym))
  (list
   'do
   (list 'let vxs (list 'cast 'Object xs))
   (list
    'if (list 'instance? 'java.util.Map vxs)
    (list '. (list 'cast 'java.util.Map vxs) 'size)
    (list
     'if (list 'string? vxs)
     (list '.length (list 'cast 'String vxs))
     (list '. (list 'cast 'java.util.Collection vxs) 'size)))))

(defn macro_get [xs k]
  (list 'y2k.RT.invoke 'y2k.prelude_java.get xs k))

(defn macro_str [& xs]
  (concat
   (list
    'String.format
    (reduce (fn* [acc x] (str acc "%s")) "" xs))
   xs))

(defn macro_string? [x]
  (list 'instance? 'String x))

(defn macro_boolean? [x]
  (list 'instance? 'Boolean (list 'cast 'Object x)))

(defn macro_println [& xs]
  (list 'do
        (list 'System.out.println
              (concat (list 'str) xs))
        (list 'y2k.RT.nop)))

(defn macro_eprintln [& xs]
  (list 'do
        (list 'System.err.println
              (concat (list 'str) xs))
        (list 'y2k.RT.nop)))

;; Regex

(defn macro_re-pattern [x]
  (list 'java.util.regex.Pattern.compile x))

(defn macro_re-find [p i]
  (list 'y2k.RT.re_find p i))

;; Collections

(defn macro_vec [xs]
  (list 'y2k.RT.vec xs))

(defn macro_list [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))

(defn macro_vector [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))

(defn macro_reduce [f init xs]
  (list 'y2k.RT.reduce f init xs))

(defn macro_second [xs]
  (list 'get xs 1))

(defn macro_vector? [xs]
  (list 'instance? 'java.util.List xs))

(defn macro_contains? [m k]
  (list 'y2k.RT.contains m k))

(defn macro_shuffle [seed xs]
  (list 'y2k.RT.shuffle seed xs))

(defn macro_take [n xs]
  (list 'y2k.RT.take n xs))
