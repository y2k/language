;; ok

(defn preserved [r]
  (if r
    (if (= (re-find r "x") "x") "" " FAIL:passage")
    " FAIL:truth"))

(defn test []
  (let [r (re-pattern "x")]
    (str "ok"
      (if (= r r) " FAIL:self-equality" "")
      (if (not= r r) "" " FAIL:self-inequality")
      (if (= r (re-pattern "x")) " FAIL:pattern-equality" "")
      (if (= r "x") " FAIL:string-right" "")
      (if (= "x" r) " FAIL:string-left" "")
      (if (not r) " FAIL:not" "")
      (if (vector? r) " FAIL:vector" "")
      (if (= (str r) "#<regex>") "" " FAIL:str")
      (if (= (str [r]) "(#<regex>)") "" " FAIL:list-str")
      (if (= (str {:r r}) "{r #<regex>}") "" " FAIL:map-str")
      (preserved r)
      (preserved ((fn [x] x) r))
      (preserved (get [r] 0))
      (preserved (get-in {:r [r]} [:r 0]))
      (preserved (let [[sequential] [r]] sequential))
      (preserved (let [{:r associative} {:r r}] associative))
      (preserved ((fn [[x]] x) [r]))
      (preserved ((fn [{:r x}] x) {:r r}))
      (preserved (deref (atom r)))
      (preserved (reset! (atom nil) r))
      (preserved (swap! (atom r) (fn [x] x))))))
