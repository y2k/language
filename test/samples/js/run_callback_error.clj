;; true|true|true
(defn test []
  (raw-code "const hostTrace = []; const hostFailure = new Error('stop'); let failure = null; let unary = true;
try {
  run_BANG_(function(item) {
    unary = unary && arguments.length === 1;
    hostTrace.push(item);
    if (item === 2) throw hostFailure;
    return 'ignored';
  }, [1, 2, 3]);
} catch (error) { failure = error; }")
  (str (= (str hostTrace) "(1 2)") "|" (= failure hostFailure) "|" unary))
