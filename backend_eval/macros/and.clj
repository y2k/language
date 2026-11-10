(def expand-and
  (fn [clauses]
    (if (= clauses (list))
      true
      (if (= (count clauses) 1)
        (get clauses 0)
        (let [name (gensym)]
          (list 'let*
                (list name (get clauses 0))
                (list 'if name (expand-and (drop 1 clauses)) name)))))))
