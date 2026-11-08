;; true|true|true|true|true|true|true
(raw-code "static String hostTrace = \"\";
static String hostValue = \"old\";
static final Exception hostFailure = new Exception(\"stop\");
static Object early() throws Exception { hostTrace += \"early\"; throw hostFailure; }
static boolean condition() { hostTrace += \"condition\"; return true; }
static String makeText() { hostTrace += \"receiver\"; return \"a\"; }
static boolean changeValue() { hostValue = \"new\"; return true; }
static void hostVoid() { hostTrace += \"void\"; }
static boolean failureCheck() { try { probe(); return false; } catch (Exception e) { return e == hostFailure; } }")
(def initialized (if true (Class/forName "java.lang.String") nil))
(defn probe [] (str (early) (if (condition) "selected" "unused") (makeText)))
(defn typed-new []
  (let [^String text (let [x (new String "a")] x)] (.concat text "b")))
(defn typed-host [] (. (cast String (let [x (String/valueOf "a")] x)) concat "b"))
(defn test []
  (let [failed (failureCheck)
        stopped (= hostTrace "early")]
    (raw-code "hostTrace = \"\";")
    (let [value (. (cast String (let [x (makeText)] x)) concat (str (if (condition) "b" "unused")))
          receiver (and (= value "ab") (= hostTrace "receivercondition"))
          callable (.call (cast java.util.concurrent.Callable
                          (let [f ^java.util.concurrent.Callable (fn [] (if true "called" nil))] f)))
          before (str hostValue (if (changeValue) hostValue "unused"))
          action ^void:java.lang.Runnable (fn [] (if true (hostVoid) (System/gc)))
          predicate ^java.util.function.Predicate (fn [^String value] (.isEmpty value))]
      (.run action)
      (str failed "|" stopped "|" receiver "|"
           (and (= (typed-new) "ab") (= (typed-host) "ab")) "|"
           (and (= callable "called") (= before "oldnew")) "|"
           (= hostTrace "receiverconditionvoid") "|"
           (and (= (.getName (cast Class initialized)) "java.lang.String")
                (= (String/valueOf (if true 42 0)) "42")
                (.test predicate ""))))))
