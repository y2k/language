;; true|true|true
(defn test []
  (str (try (throw false) (catch default (if default false true))) "|"
       (try (throw false) (catch arguments (if arguments false true))) "|"
       (try (throw false) (catch truthy (if truthy false true)))))
