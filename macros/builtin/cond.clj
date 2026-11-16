(def expand-cond
  (fn [clauses]
    (if (= clauses (list))
      nil
      (if (= (drop 1 clauses) (list))
        (macro-error "cond: expected test/result pairs")
        (list 'if
              (get clauses 0)
              (get clauses 1)
              (expand-cond (drop 2 clauses)))))))
