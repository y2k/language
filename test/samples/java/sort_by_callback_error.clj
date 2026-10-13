;; true
(defn test []
  (raw-code "StringBuilder trace = new StringBuilder();
Exception failure = new Exception(\"stop\");
Exception caught = null;
try {
  sort_by((Fn1) (item -> {
    trace.append(item);
    if (item.equals(2)) throw failure;
    return item;
  }), list(1, 2, 3));
} catch (Exception error) { caught = error; }
boolean stopped = caught == failure && trace.toString().equals(\"12\");")
  (str stopped))
