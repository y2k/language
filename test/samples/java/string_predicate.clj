;; true|true|true|true|true|true|false

(defn test []
  (str (string? 'foo) "|" (string? 'nil) "|" (string? 'false)
       "|" (string? '42) "|" (string? '"text")
       "|" (string? (java.lang.String. "text"))
       "|" (string? (java.lang.StringBuilder. "text"))))
