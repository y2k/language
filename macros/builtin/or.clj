(def expand-or
  (fn [clauses]
    (if (= clauses (list))
      nil
      (if (= (count clauses) 1)
        (get clauses 0)
        (let [name (gensym)]
          (list 'let*
                (list name (get clauses 0))
                (list 'if name name (expand-or (drop 1 clauses)))))))))
