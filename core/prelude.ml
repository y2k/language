(* THIS FILE IS GENERATED *)

let java_runtime = {|
package y2k;

import java.util.*;

public class RT {

    public static boolean toBoolean(Object x) {
        if (x instanceof Boolean) {
            return (Boolean) x;
        }
        return x != null;
    }

    public static Object eprintln(Object... xs) {
        for (Object x : xs) {
            System.err.print(x);
            System.err.print(" ");
        }
        System.err.println();
        return null;
    }

    // Exceptions

    private static <T extends Throwable> void throwException(Throwable exception, Object dummy) throws T {
        throw (T) exception;
    }

    public static <T> T throwSilent(Object exception) {
        throwException((Throwable) exception, null);
        return null;
    }

    // Collections

    public static List<Object> take(Object n, Object xs) {
        var col = (List<Object>) xs;
        return col.subList(0, (Integer) n);
    }

    public static List<Object> shuffle(Object seed, Object xs) {
        var col = (Collection<Object>) xs;
        var result = new ArrayList<>(col);
        var seed2 = (long) (((double) seed) * Long.MAX_VALUE);
        Collections.shuffle(result, new Random(seed2));
        return result;
    }

    public static List<Object> conj(Object xs, Object x) {
        var col = (Collection<Object>) xs;
        var result = new ArrayList<>(col);
        result.add(x);
        return result;
    }

    public static <T> T get(Object source, Object key) {
        if (source instanceof java.util.Map) {
            return (T) ((java.util.Map<?, ?>) source).get(key);
        }
        if (source instanceof java.util.List) {
            return (T) ((java.util.List<?>) source).get((Integer) key);
        }
        throw new RuntimeException("Unsupported source: " + source + ", key: " + key);
    }

    public static List<Object> map(Object f, Object xs) {
        var func = (java.util.function.Function<Object, Object>) f;
        if (xs instanceof Map) {
            var map = (Map<Object, Object>) xs;
            var result = new ArrayList<Object>(map.size());
            for (Map.Entry<Object, Object> entry : map.entrySet()) {
                result.add(func.apply(List.of(entry.getKey(), entry.getValue())));
            }
            return result;
        }
        var col = (Collection<Object>) xs;
        var result = new ArrayList<Object>(col.size());
        for (Object x : col) {
            result.add(func.apply(x));
        }
        return result;
    }

    public static Object drop(Object n, Object xs) {
        var col = (List<Object>) xs;
        return col.subList((Integer) n, col.size());
    }

    public static java.util.Map<Object, Object> merge(Object as, Object bs) {
        var a = (java.util.Map<Object, Object>) as;
        var b = (java.util.Map<Object, Object>) bs;
        var result = new java.util.HashMap<>(a);
        result.putAll(b);
        return result;
    }

    public static Object concat(Object xs, Object ys) {
        var a = (java.util.List<Object>) xs;
        var b = (java.util.List<Object>) ys;
        var result = new java.util.ArrayList<>(a);
        result.addAll(b);
        return result;
    }

    public static Object[] into_array(Class<?> cls, Object xs) {
        var col = (java.util.List<Object>) xs;
        var result = (Object[]) java.lang.reflect.Array.newInstance(cls, col.size());
        return col.toArray(result);
    }

    public static Object assoc(Object xs, Object k, Object v) {
        var col = (java.util.Map<Object, Object>) xs;
        var result = new java.util.HashMap<>(col);
        result.put(k, v);
        return result;
    }

    // Lambda

    public interface Fn0 {
        public Object invoke() throws Exception;
    }

    public interface Fn1 extends java.util.function.UnaryOperator<Object> {
        public Object invoke(Object a) throws Exception;

        public default Object apply(Object a) {
            try {
                return invoke(a);
            } catch (Exception e) {
                return throwSilent(e);
            }
        }
    }

    public interface Fn2 {
        public Object invoke(Object a, Object b) throws Exception;
    }

    public interface Fn3 {
        public Object invoke(Object a, Object b, Object c) throws Exception;
    }

    public interface Fn4 {
        public Object invoke(Object a, Object b, Object c, Object d) throws Exception;
    }

    public static Object fn(Fn0 f) {
        return new Fn0() {

            @Override
            public Object invoke() throws Exception {
                return f.invoke();
            }

            @Override
            public String toString() {
                return "lambda0";
            }
        };
    }

    public static Object fn(Fn1 f) {
        return new Fn1() {

            @Override
            public Object invoke(Object a) throws Exception {
                return f.invoke(a);
            }

            @Override
            public String toString() {
                return "lambda1";
            }
        };
    }

    public static Object fn(Fn2 f) {
        return new Fn2() {

            @Override
            public Object invoke(Object a, Object b) throws Exception {
                return f.invoke(a, b);
            }

            @Override
            public String toString() {
                return "lambda2";
            }
        };
    }

    public static Object fn(Fn3 f) {
        return new Fn3() {

            @Override
            public Object invoke(Object a, Object b, Object c) throws Exception {
                return f.invoke(a, b, c);
            }

            @Override
            public String toString() {
                return "lambda3";
            }
        };
    }

    public static Object fn(Fn4 f) {
        return new Fn4() {

            @Override
            public Object invoke(Object a, Object b, Object c, Object d) throws Exception {
                return f.invoke(a, b, c, d);
            }

            @Override
            public String toString() {
                return "lambda4";
            }
        };
    }

    public static Object invoke(Object f) {
        try {
            return ((Fn0) f).invoke();
        } catch (Exception e) {
            return throwSilent(e);
        }
    }

    public static Object invoke(Object f, Object a) {
        try {
            return ((Fn1) f).invoke(a);
        } catch (Exception e) {
            return throwSilent(e);
        }
    }

    public static Object invoke(Object f, Object a, Object b) {
        try {
            return ((Fn2) f).invoke(a, b);
        } catch (Exception e) {
            return throwSilent(e);
        }
    }

    public static Object invoke(Object f, Object a, Object b, Object c) {
        try {
            return ((Fn3) f).invoke(a, b, c);
        } catch (Exception e) {
            return throwSilent(e);
        }
    }

    public static Object invoke(Object f, Object a, Object b, Object c, Object d) {
        try {
            return ((Fn4) f).invoke(a, b, c, d);
        } catch (Exception e) {
            return throwSilent(e);
        }
    }
}
|}

let prelude_java_macro = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(defn macro_boolean [x]
  (list 'y2k.RT.toBoolean x))

(defn macro_unixtime []
  (list '/ (list '.getTime (list 'java.util.Date.)) 1000.0))

(defn list [& xs] xs)

(defn macro_conj [xs x]
  (list 'y2k.RT.conj xs x))

(defn macro_map [f xs]
  (list 'y2k.RT.map f xs))

(defn macro_eprintln [& xs]
  (concat (list 'y2k.RT.eprintln) xs))

(defn macro_FIXME [& xs]
  (list 'java.util.Objects.requireNonNull
        nil
        (concat
         (list 'str)
         xs)))

(defn macro_not= [x y]
  (list 'not (list '= x y)))

(defn macro_rest [xs]
  (let [xs_var (gensym)]
    (list
     'do
     (list
      'let* xs_var (list 'cast 'java.util.List xs))
     (list '.subList xs_var 1 (list '.size xs_var)))))

(defn macro_concat [& xs]
  (concat (list 'y2k.RT.concat)
          xs))

(defn macro_merge [m1 m2]
  (list 'y2k.RT.merge m1 m2))

(defn macro_drop [n xs]
  (list 'y2k.RT.drop n xs))

(defn macro_first [xs]
  (list 'get xs 0))

(defn macro_empty? [xs]
  (list '= 0 (list 'count xs)))

(defn macro_some? [x]
  (list 'not (list 'nil? x)))

(defn macro_nil? [x]
  (list '= 'nil x))

(defn macro_into-array2 [class col]
  (list 'y2k.RT.into_array class col))

(defn macro_assoc [xs k v]
  (list 'y2k.RT.assoc xs k v))

(defn macro_atom [x]
  (list 'java.util.concurrent.atomic.AtomicReference. x))

(defn macro_reset! [a x]
  (let [var (gensym)]
;;
    (list 'let (list 'vector var x)
          (list '.set
                (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
                var)
          var)
;;
    ))

;; (defn macro_reset! [a x]
;;   (list
;;    '.set
;;    (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
;;    x))

(defn macro_swap! [a f]
  (list
   '.getAndUpdate
   (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
   (list 'cast 'java.util.function.UnaryOperator f)))

(defn macro_deref [a]
  (list
   '.get
   (list 'cast 'java.util.concurrent.atomic.AtomicReference a)))

(defn macro_comment [x] (list 'do))

(defn macro_defn- [name args & body]
  (concat (list 'defn name args) body))

(defn macro_= [x y]
  (list 'java.util.Objects.equals x y))

(defn macro_hash-map [& xs]
  (concat
   (list 'java.util.Map.of)
   xs))

(defn macro_count [xs]
  (let* vxs (gensym))
  (list
   'do
   (list 'let vxs xs)
   (list
    'if
    (list 'instance? 'java.util.Map vxs)
    (list '. (list 'cast 'java.util.Map vxs) 'size)
    (list '. (list 'cast 'java.util.Collection vxs) 'size))))

(defn macro_get [xs k]
  (list 'y2k.RT.get xs k))

;; (defn macro_get [xs i]
;;   (let* vxs (gensym))
;;   (let* vi (gensym))
;;   (list
;;    'do
;;    (list 'let vxs xs)
;;    (list 'let vi i)
;;    (list 'if
;;          (list 'instance? 'java.util.Map vxs)
;;          (list '. (list 'cast 'java.util.Map vxs) 'get vi)
;;          (list '. (list 'cast 'java.util.List vxs) 'get (list 'cast 'int (list 'cast 'Object vi))))))

(defn macro_str [& xs]
  (concat
   (list
    'String.format
    (reduce (fn* [acc x] (str acc "%s")) "" xs))
   xs))

(defn macro_vector [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))

;; Collections

(defn macro_shuffle [seed xs]
  (list 'y2k.RT.shuffle seed xs))

(defn macro_take [n xs]
  (list 'y2k.RT.take n xs))

|}

let prelude_eval_macro = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(defn macro_comment [x]
  (list 'do))
|}

let prelude_eval = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(def* vector (fn* [& xs] xs))

(defn not [x] (if x false true))

(defn nil? [x] (= x nil))
(defn some? [x] (not (nil? x)))

|}

let prelude_js_macro = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

(defn macro_boolean [x] x)

(defn macro_comment [x]
  (list 'do))

(defn macro_println [& xs]
  (concat (list 'console.log) xs))

(defn macro_eprintln [& xs]
  (concat (list 'console.error) xs))

(defn macro_str [& xs]
  (concat (list '+ "") xs))

(defn macro_nil? [x]
  (list '= x nil))

(defn macro_fn? [x]
  (list '= (list 'type x) "function"))

(defn macro_string? [x]
  (list '= (list 'type x) "string"))

(defn macro_number? [x]
  (list '= (list 'type x) "number"))

(defn macro_boolean? [x]
  (list '= (list 'type x) "boolean"))

;; (defn macro_list? [x]
;;   (list '= :list (list 'get x :type)))

(defn macro_vector? [x]
  (list 'Array.isArray x))

(defn macro_some? [x]
  (list 'not (list '= x nil)))

(defn macro_FIXME [& xs]
  (list
   (list 'fn
         (vector)
         (list 'throw
               (list 'Error.
                     (concat (list 'str) xs)))
         nil)))

(defn macro_unixtime []
  (list '/ (list 'Date.now) 1000))

;; Regex

(defn macro_re-pattern [x]
  (list 'RegExp. x))

;; Collections

(defn macro_rest [xs]
  (list '.slice xs 1))

(defn macro_last [xs]
  (list '.at xs -1))

(defn macro_conj [xs x]
  (list 'concat xs (list 'vector x)))

(defn macro_first [xs]
  (list 'get xs 0))

;; (defn macro_list [& xs]
;;   (let [v (gensym)]
;;     (list 'let (vector v (vec xs))
;;           (hash-map
;;            :type :list
;;            :items v
;;            :length (count v)
;;            'Symbol.iterator (list 'get v 'Symbol.iterator)))))

(defn macro_reduce [f init xs]
  (let [v (gensym)]
    (list 'let [v xs]
          (list 'if (list 'vector? v)
                (list '.reduce v f init)
                (list '.reduce (list 'Object.entries v) f init)))))

(defn macro_concat [& xs]
  (concat
   (list
    '.concat
    [])
   xs))

(defn macro_empty? [xs]
  (list '= 0 (list '.-length xs)))

(defn macro_count [xs]
  (list '.-length xs))

;; Atoms

(defn macro_swap! [a f]
  (list 'do
        (list 'assoc! a 0 (list f (list 'get a 0)))
        (list 'get a 0)))

(defn macro_reset! [a x]
  (list 'assoc! a 0 x))

(defn macro_atom [x]
  (list 'vector x))

(defn macro_deref [a]
  (list 'get a 0))
|}
