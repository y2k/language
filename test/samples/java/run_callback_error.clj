;; true|true
(defn test []
  (raw-code "StringBuilder hostTrace = new StringBuilder(); Exception hostFailure = new Exception(\"stop\"); Exception failure = null;
try {
  run_BANG_((Fn1) (item -> {
    hostTrace.append(item);
    if (item.equals(2)) throw hostFailure;
    return \"ignored\";
  }), list(1, 2, 3));
} catch (Exception error) { failure = error; }")
  (str (= (.toString hostTrace) "12") "|" (= failure hostFailure)))
