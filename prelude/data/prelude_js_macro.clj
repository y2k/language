(defn macro_* [a b]
  (list '__inline_op__ "*" a b))

(defn macro_mod [a b]
  (list '__inline_op__ "%" a b))

(defn macro_> [a b]
  (list '__inline_op__ ">" a b))

(defn macro_>= [a b]
  (list '__inline_op__ ">=" a b))

(defn macro_< [a b]
  (list '__inline_op__ "<" a b))

(defn macro_<= [a b]
  (list '__inline_op__ "<=" a b))

(defn macro_+ [& xs]
  (concat (list 'prelude/+) xs))

(defn macro_- [& xs]
  (concat (list 'prelude/_MINUS_) xs))

;;

(defn macro_hash-map-from [xs]
  (list 'prelude/hash_map_from xs))

(defn macro_assert [a b]
  (list 'prelude/debug_assert a b))

(defn macro_not= [x y]
  (list 'not (list '= x y)))

(defn macro_parse-int [s]
  (list 'parseInt s))

(defn macro_subs [s sp ep]
  (list '.substring s sp ep))

(defn macro_boolean [x] x)

(defn macro_comment [x]
  (list 'do))

(defn macro_println [& xs]
  (concat (list 'console.log) xs))

(defn macro_eprintln [& xs]
  (concat (list 'console.error) xs))

(defn macro_str [& xs]
  (concat (list '+ "") xs))

(defn macro_nil? [x]
  (list '= x nil))

(defn macro_fn? [x]
  (list '= (list 'type x) "function"))

(defn macro_string? [x]
  (list '= (list 'type x) "string"))

(defn macro_number? [x]
  (list '= (list 'type x) "number"))

(defn macro_boolean? [x]
  (list '= (list 'type x) "boolean"))

(defn macro_merge [m1 m2]
  (list 'Object.assign (list 'hash-map) m1 m2))

(defn macro_vector? [x]
  (list 'Array.isArray x))

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

(defn macro_unixtime []
  (list '/ (list 'Date.now) 1000))

;; Strings

(defn macro_string/join [sep xs]
  (list '.join xs sep))

(defn macro_string/split [s sep]
  (list '.split s sep))

(defn macro_string/starts-with? [s prefix]
  (list '.startsWith s prefix))

(defn macro_clojure.string/ends-with? [s suffix]
  (list '.endsWith s suffix))

(defn macro_clojure.string/replace [s match replacement]
  (list '.replaceAll s match replacement))

;; Regex

(defn macro_re-pattern [x]
  (list 'RegExp. x))

;; (defn macro_re-find [p i]
;;   (list '.exec p i))

(defn macro_re-find [p i]
  (list 'prelude/re_find p i))

;; Collections

(defn macro_vec [x] x)

(defn macro_list [& xs]
  (concat
   (list 'vector)
   xs))

(defn macro_map [f xs]
  (list '.map xs f))

(defn macro_filter [f xs]
  (list '.filter xs f))

(defn macro_second [xs]
  (list 'get xs 1))

(defn macro_contains? [xs x]
  (list '.hasOwnProperty xs x))

(defn macro_rest [xs]
  (list '.slice xs 1))

(defn macro_last [xs]
  (list '.at xs -1))

(defn macro_conj [xs x]
  (list 'concat xs (list 'vector x)))

(defn macro_first [xs]
  (list 'get xs 0))

;; (defn macro_list [& xs]
;;   (let [v (gensym)]
;;     (list 'let (vector v (vec xs))
;;           (hash-map
;;            :type :list
;;            :items v
;;            :length (count v)
;;            'Symbol.iterator (list 'get v 'Symbol.iterator)))))

(defn macro_reduce [f init xs]
  (let [v (gensym)]
    (list 'let [v xs]
          (list 'if (list 'vector? v)
                (list '.reduce v f init)
                (list '.reduce (list 'Object.entries v) f init)))))

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

(defn macro_swap! [a f]
  (list 'prelude/swap! a f))

(defn macro_reset! [a x]
  (list 'assoc! a 0 x))

(defn macro_atom [x]
  (list 'vector x))

(defn macro_deref [a]
  (list 'get a 0))