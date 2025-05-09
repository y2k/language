;;

(def* list
  (fn* [& xs] xs))

(def* macro_str
  (fn* [& xs]
       (concat
        (list
         (quote* String.format)
         (reduce (fn* [acc x] (str acc "%s")) "" xs))
        xs)))

(def* macro_vector
  (fn* [& xs]
       (concat
        (list (quote* java.util.Arrays.asList))
        xs)))

;;
