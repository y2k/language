;; true|true

(raw-code "static String trace = \"\";
static void note(String s) { trace += s; }
static AutoCloseable resource(String name) {
  note(\"open\" + name + \"/\");
  return () -> note(\"close\" + name + \"/\");
}")

(defn factory [name] (resource (cast String name)))

(defn test []
  (let [result (with-open [a (or nil (factory "a"))]
                 (with-open [b (and a (factory "b"))]
                   (note "body/")
                   (or false 42)))]
    (str (= result 42) "|"
         (= trace "opena/openb/body/closeb/closea/"))))
