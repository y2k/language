;; true
(defn test []
  (raw-code "boolean stopped = true;
for (Fn2 operation : new Fn2[] {y2k.language.language_runtime::filter, y2k.language.language_runtime::keep}) {
  StringBuilder trace = new StringBuilder();
  Exception failure = new Exception(\"stop\");
  Exception caught = null;
  try {
    operation.call((Fn1) (item -> {
      trace.append(item);
      if (item.equals(2)) throw failure;
      return item;
    }), list(1, 2, 3));
  } catch (Exception error) { caught = error; }
  stopped = stopped && caught == failure && trace.toString().equals(\"12\");
}")
  (str stopped))
