(ns im.y2k.chargetimer
  (:import [android.content Context]
           [com.google.gson Gson]
           [java.io File Reader FileReader InputStreamReader]
           [java.util List Objects]
           [java.util.function Function]))

;; (__unsafe_inject_code "data class Env(val bindings: Map<String, Object> = emptyMap())")

(defn run_code [^Context context webview ^String event]
  (let [^File f1 (File. (.getFilesDir context) "sample.json")
        ^Reader json (if (.exists f1) (checked! (FileReader. f1)) (InputStreamReader. (checked! (.open (.getAssets context) "sample.json"))))
        code (.fromJson (Gson.) json List.class)
        ^"List<List<Object>>" event_handlers (inter (Env.) code)
        ^"Function<Object,Object>" handler (get (.anyMatch (.stream event_handlers) (fn [[name]] (= name event))) 1)]
    (.apply handler [{:context context :webview webview}])))

(defn ^Object inter [^Env env ^Object node]
  (if (is node List<*>) s
      (let [args (.subList node 1 node.size)] ≠
           (case (get node 0)
             "hash-map" (->
                         args
                         (.chunked 2)
                         (.map (fn [[k v]] (Pair. (inter env k) (inter env v))))
                         (.toMap))

             "vector" (.map args (fn [x] (inter env x)))

             "get" (let [[target i] args]
                     (__prelude_getm (inter env target) (as (inter env i) String)))

             "let*" (let [[binding] args]
                      (defn ^Object loop [^Env env ^"List<Object>" xs]
                        (if (= 0 xs.size)
                          env
                          (let [[kn vn] xs
                                k (str kn)
                                v (inter env vn)]
                            (loop
                             (__unsafe_inject_code "env.copy(bindings = env.bindings.plus(k to v))")
                              (.drop xs 2)))))
                      (let [env (as (loop env (as binding "List<Object>")) Env)
                            body (.drop args 1)]
                        (defn ^Object loop [^"List<Object>" xs]
                          (if (= 1 xs.size)
                            (inter env (get xs 0))
                            (do
                              (inter env (get xs 0))
                              (loop (.drop xs 1)))))
                        (loop body)))

             "fn*" (let [args_node (as (get args 0) "List<String>")
                         body (.subList args 1 args.size)]
                     (fn [^"List<Object>" args]
                       (let [env (Env. (.plus env.bindings (.toMap (.zip args_node args))))]
                         (.fold body (as null "Object") (fn [_ node] (inter env node))))))

             "if" (let [[cond then_ else_] args]
                    (if (= true (inter env cond)) (inter env then_) (inter env else_)))

             "new" (let [[source] args]
                     (let [args (-> args (.drop 1) (.map (fn [x] (inter env x))))
                           cls (Class/forName (.replace (as source String) "_" ""))
                           index (.count source (fn [x] (= x (get "_" 0))))]
                       (-> cls.constructors
                           (.filter (fn [x] (= x.parameterCount args.size)))
                           (get index)
                           (.newInstance (spread (.toTypedArray args))))))

             "." (let [[source method] args]
                   (let [args (-> args (.drop 2) (.map (fn [x] (inter env x))))
                         real_method (.replace (as method String) "_" "")
                         i (inter env source)
                         cls (if (not= null i)
                               i.javaClass
                               (Class/forName (as source String)))
                         index (.count (as method String) (fn [x] (= x (get "_" 0))))]
                     (-> cls.methods
                         (.filter (fn [x] (and
                                           (or (not= i null) (java.lang.reflect.Modifier/isStatic (.getModifiers x)))
                                           (= x.name real_method))))
                         (.filter (fn [x] (= x.parameterCount args.size)))
                         (get index)
                         (.invoke i (spread (.toTypedArray args))))))

             (let [f (as (.getOrElse env.bindings (as (get node 0) String) (fn [] (error node))) "(Object) -> Object")]
               (f (.map args (fn [x] (inter env x)))))))

      (let [value (as node String)]
        (cond
          (.startsWith value "\"") (.replace (.substring value 1 (- value.length 1)) "\\\"" "\"")
          (not= null (.toIntOrNull value)) (.toInt value)
          :else (__prelude_getm env.bindings value)))))
