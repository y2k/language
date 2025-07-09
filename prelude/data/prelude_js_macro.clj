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