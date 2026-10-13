;; true true

(defn test []
  (raw-code "boolean errors = true;
Object empty = hash_map();
for (Object value : new Object[] {null, list(), 42, false, \"text\"}) {
  for (Fn0 action : new Fn0[] {
      () -> assoc(value, \"x\", 1), () -> merge(value, empty), () -> merge(empty, value)}) {
    boolean rejected = false;
    try { action.call(); } catch (RuntimeException expected) { rejected = true; }
    errors = errors && rejected;
  }
}
boolean fresh = merge(empty, empty) != empty && assoc(empty, \"x\", 1) != empty;")
  (str errors " " fresh))
