(def expand-with-open-bindings
  (fn [source bindings body]
    (if (= (count bindings) 0)
      (if (= (count body) 0) nil (concat (list 'do) body))
      (if (< (count bindings) 2)
        (macro-error "with-open: expected simple symbol/value pairs")
        (let [name (get bindings 0)]
          (if (syntax-simple-symbol? name)
            (list 'let
                  (syntax-list-at source (list name (get bindings 1)))
                  (list 'try
                        (expand-with-open-bindings source (drop 2 bindings) body)
                        (list 'finally
                              (list '.close (list 'cast 'java.lang.AutoCloseable name)))))
            (macro-error "with-open: expected simple symbol/value pairs")))))))

(def expand-with-open
  (fn [args]
    (let [bindings (get args 0)]
      (if (syntax-vector? bindings)
        (expand-with-open-bindings bindings (syntax-children bindings) (drop 1 args))
        (macro-error "with-open: expected a binding vector")))))
