;; Collections

(defn get [xs i]
  (cond
    (instance? java.util.Map xs) (.get (cast java.util.Map xs) i)
    (instance? java.util.List xs) (.get (cast java.util.List xs) (cast int i))
    :else (FIXME "Unsupported source: " (str xs) ", key: " (str i))))

;; (defn macro_FIXME [& xs]
;;   (list 'java.util.Objects.requireNonNull
;;         nil
;;         (list 'string/join
;;               " "
;;               (concat (list 'vector) xs))))

(defn fixme [loc xs]
  (java.util.Objects.requireNonNull
   nil
   (str loc " " xs)))
