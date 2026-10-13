;; false|false|false|false|true

(defn test []
  (str (string? 'foo) "|" (string? 'nil) "|" (string? 'false)
       "|" (string? '42) "|" (string? '"text")))
