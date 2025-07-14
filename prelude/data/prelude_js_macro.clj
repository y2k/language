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

;; (defn macro_list? [x]
;;   (list '= :list (list 'get x :type)))

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

;; Regex

(defn macro_re-pattern [x]
  (list 'RegExp. x))

;; Collections

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
  (list 'do
        (list 'assoc! a 0 (list f (list 'get a 0)))
        (list 'get a 0)))

(defn macro_reset! [a x]
  (list 'assoc! a 0 x))

(defn macro_atom [x]
  (list 'vector x))

(defn macro_deref [a]
  (list 'get a 0))