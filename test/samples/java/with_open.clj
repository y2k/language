;; true|true|true|true|true|true|true|true
(raw-code "static String trace = \"\";
static void note(String s) { trace += s; }
static AutoCloseable resource(String name) {
  note(\"open\" + name + \"/\");
  return () -> note(\"close\" + name + \"/\");
}")
(defn factory [name] (resource (cast String name)))
(defn dependent [parent] (if (instance? java.lang.AutoCloseable parent) (factory "b") nil))
(defn test []
  (let [result (with-open [a (factory "a") b (dependent a)] (note "body/") 42)
        ordered (= trace "opena/openb/body/closeb/closea/")
        reader (with-open [^java.io.Reader r (new java.io.StringReader "A")] (.read r))
        empty (= (with-open []) nil)
        value (= (with-open [] 42) 42)
        false-value (with-open [r (factory "f")] (note "false/") false)
        nil-value (with-open [r (factory "n")] nil)
        no-body (with-open [r (factory "e")])]
    (raw-code "trace = \"\";")
    (with-open [r (factory "d")] (note "discard/"))
    (note "after/")
    (str (and (= result 42) ordered) "|" (= reader 65) "|" empty "|" value "|"
         (and (= false-value false) (not false-value)) "|"
         (and (= nil-value nil) (not nil-value)) "|" (= no-body nil) "|"
         (and (= (with-open [r (factory "v")] 7) 7)
              (= trace "opend/discard/closed/after/openv/closev/")))))
