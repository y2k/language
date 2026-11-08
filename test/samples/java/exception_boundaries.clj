;; true|true|true|true|true|true
(gen-class :name Items :extends java.util.ArrayList :methods [[clear [] void]])
(raw-code "static String trace = \"\";
static final Exception failure = new Exception(\"checked\");
static final Error fatal = new Error(\"fatal\");
static Object checked() throws Exception { throw failure; }
static Object crash() { throw fatal; }
static void hostVoid() { trace += \"v\"; }
static boolean identical(Object a, Object b) { return a == b; }
static boolean errorCheck() throws Exception {
  try { errorProbe(); return false; } catch (Error e) { return e == fatal; }
}")
(defn -clear [this] (try (checked) (finally (hostVoid))))
(defn errorProbe [] (try (crash) (catch e false) (finally (hostVoid))))
(defn test []
  (let [nonfinal (try (checked) (catch e 1 2))
        action ^void:java.lang.Runnable (fn [] (try (hostVoid) (finally (hostVoid))))
        value ^java.util.concurrent.Callable (fn [] (try (checked) (finally (hostVoid))))
        checked-action ^void:java.lang.Runnable (fn [] (try (checked) (finally (hostVoid))))]
    (try (hostVoid) (finally (hostVoid)))
    (.run action)
    (let [called (try (.call value) (catch e (identical e failure)))
          ran (try (.run checked-action) false (catch e (identical e failure)))
          bridge (try (.clear (new Items)) false (catch e (identical e failure)))
          error (errorCheck)]
      (str (= nonfinal 2) "|" called "|" ran "|" bridge "|" error "|" (= trace "vvvvvvvv")))))
