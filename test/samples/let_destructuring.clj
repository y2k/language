;; a-b-c-Ada-nil

(defn test []
  (let [[a [b c] {:name n :missing m}] (list "a" (list "b" "c") (hash-map "name" "Ada"))
        summary (str a "-" b "-" c "-" n "-" m)]
    summary))
