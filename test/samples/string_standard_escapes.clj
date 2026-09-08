;; <div data-post="serbia/4"></div>|\|\n

(def value "<div data-post=\"serbia/4\"></div>")

(defn test []
  (str value "|" "\\" "|" "\\n"))
