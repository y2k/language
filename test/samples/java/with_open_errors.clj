;; true|true|true|true|true|true|true|true
(raw-code "static String trace = \"\";
static final Exception opening = new Exception(\"open\");
static final Exception bodyError = new Exception(\"body\");
static final Exception closeA = new Exception(\"a\");
static final Exception closeB = new Exception(\"b\");
static void note(String s) { trace += s; }
static boolean identical(Object a, Object b) { return a == b; }
static boolean unsuppressed(Object e) { return ((Throwable)e).getSuppressed().length == 0; }
static AutoCloseable resource(String name, Object failure) {
  note(\"open\" + name + \"/\");
  return () -> { note(\"close\" + name + \"/\"); if (failure != null) throw (Exception)failure; };
}")
(defn factory [name failure] (resource (cast String name) failure))
(defn bad-resource [] "text")
(defn check-case [open-fails body-fails a-error b-error]
  (raw-code "trace = \"\";")
  (let [caught (try
                 (with-open [a (factory "a" a-error)
                             b (if open-fails (do (note "openb/") (throw opening)) (factory "b" b-error))]
                   (note "body/")
                   (if body-fails (throw bodyError) 42))
                 (catch e e))
        expected (if a-error a-error (if b-error b-error (if open-fails opening bodyError)))]
    (and (identical caught expected) (unsuppressed caught)
         (= trace (if open-fails "opena/openb/closea/" "opena/openb/body/closeb/closea/")))))
(defn invalid-resource [null-resource]
  (raw-code "trace = \"\";")
  (let [caught (try
                 (with-open [a (factory "a" nil) b (if null-resource nil (bad-resource))]
                   (note "body/") nil)
                 (catch e e))]
    (and (= trace "opena/body/closea/")
         (if null-resource (instance? java.lang.NullPointerException caught)
             (instance? java.lang.ClassCastException caught)))))
(defn test []
  (str (check-case true false nil nil) "|"
       (check-case false true nil nil) "|"
       (check-case false true nil closeB) "|"
       (check-case false true closeA closeB) "|"
       (check-case false false nil closeB) "|"
       (check-case true false closeA nil) "|"
       (invalid-resource true) "|" (invalid-resource false)))
