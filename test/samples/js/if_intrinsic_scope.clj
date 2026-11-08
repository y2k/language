;; ab|cd|local
(def Reflect "module")
(def globalThis "module")
(defn test []
  (let [Reflect "local" globalThis "local"]
    (raw-code "if (Reflect !== 'local' || globalThis !== 'local') throw new Error('local names changed');")
    (str (. "a" concat (if true "b" "unused")) "|"
         (. "c" concat (if true "d" "unused")) "|" Reflect)))
