;; absent text absent text absent text

(defn read-keys [{nil absent "nil" text}]
  (str absent " " text))

(defn test []
  (let [values (hash-map nil "absent" "nil" "text")
        {nil absent "nil" text} values]
    (str (get values nil) " " (get values "nil") " "
         absent " " text " " (read-keys values))))
