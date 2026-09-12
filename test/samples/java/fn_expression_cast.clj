;; cast|ok

(defn make-effect [] (fn [value] value))

(defn test []
  (let [x ^java.util.concurrent.Callable (fn [] "ok")]
    (str ((cast Fn1 (make-effect)) "cast") "|"
         (.call (cast java.util.concurrent.Callable x)))))
