;; true|true
(ns checks.callbacks.core)

(defn duplicate [item] (str item item))

(defn test []
  (let [named (map duplicate [1 2])
        callback duplicate
        local (map callback [3])]
    (str (= (str named) "(11 22)") "|" (= (get local 0) "33"))))
