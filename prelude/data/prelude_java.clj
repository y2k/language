(ns prelude_java)

(defn inc [^int x]
  (+ x 1))

(defn fixme [loc xs]
  (java.util.Objects.requireNonNull
   nil
   (str loc " " xs)))

;; Collections

(defn get [xs i]
  (cond
    (instance? java.util.Map xs) (.get (cast java.util.Map xs) i)
    (instance? java.util.List xs) (.get (cast java.util.List xs) (cast int i))
    :else (FIXME "Unsupported source: " (str xs) ", key: " (str i))))

(defn update [m k f]
  (assoc m k (f (get m k))))
