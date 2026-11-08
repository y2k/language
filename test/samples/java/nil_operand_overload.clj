;; String|String|di
(raw-code "static String trace = \"\";
static String pick(String value, Object other) { return \"String\"; }
static String pick(Object value, Object other) { return \"Object\"; }
static boolean mark(String tag) { trace += tag; return true; }")
(defn test []
  (let [with-do (pick nil (do (mark "d") 1))
        with-if (pick nil (if (mark "i") 1 2))]
    (str with-do "|" with-if "|" trace)))
