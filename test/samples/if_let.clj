;; ab|first|later|zero|nil

(defn success []
  (if-let [x "a"
           y (str x "b")]
    y
    (missing)))

(defn first-false []
  (if-let [x false
           y (missing)]
    (missing)
    "first"))

(defn later-false []
  (if-let [x true
           y nil
           z (missing)]
    (missing)
    "later"))

(defn zero-truthy []
  (if-let [x 0]
    "zero"
    (missing)))

(defn default-nil []
  (if-let [x false]
    (missing)))

(defn test []
  (str (success) "|" (first-false) "|" (later-false) "|" (zero-truthy) "|" (default-nil)))
