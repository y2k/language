;; true|true|true|true|true|true|true|false|false|false|false|false|false|false|false

(defn test []
  (str (string? 'foo) "|" (string? 'nil) "|" (string? 'false)
       "|" (string? '42) "|" (string? '"text")
       "|" (string? (.parse JSON "\"text\""))
       "|" (string? (.parse JSON "\"\""))
       "|" (string? (.parse JSON "null"))
       "|" (string? (.parse JSON "42"))
       "|" (string? (.parse JSON "false"))
       "|" (string? (.parse JSON "[]"))
       "|" (string? (.parse JSON "{}"))
       "|" (string? (String. "text"))
       "|" (string? undefined)
       "|" (string? (Object.))))
