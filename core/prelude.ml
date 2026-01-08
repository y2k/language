(* THIS FILE IS GENERATED *)

let java_runtime = {|
package y2k;

import java.util.*;

@SuppressWarnings("unchecked")
public class RT {

    public static Object hash_map_from(Object xs) {
        var result = new HashMap<Object, Object>();
        var items = (java.util.List<Object>) xs;
        for (int i = 0; i < items.size(); i += 2) {
            result.put(items.get(i), items.get(i + 1));
        }
        return result;
    }

    public static Object nop() {
        return null;
    }

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

    // Regex

    public static Object re_find(Object re, Object s) {
        var pattern = (java.util.regex.Pattern) re;
        var matcher = pattern.matcher((CharSequence) s);
        if (matcher.find()) {
            return matcher.group();
        } else {
            return null;
        }
    }

    // Collections

    public static java.util.List<?> vec(Object xs) {
        if (xs instanceof Collection) {
            return (java.util.List<?>) xs;
        } else if (xs instanceof Object[]) {
            return Arrays.asList((Object[]) xs);
        }
        throw new RuntimeException("Unsupported source: " + xs);
    }

    public static Object hash_map(Object... xs) {
        var result = new HashMap<Object, Object>();
        for (int i = 0; i < xs.length; i += 2) {
            result.put(xs[i], xs[i + 1]);
        }
        return result;
    }

    public static Object reduce(Object f, Object init, Object xs) {
        var func = (Fn2) f;
        var col = (Collection<Object>) xs;
        var result = init;
        for (Object x : col) {
            try {
                result = func.invoke(result, x);
            } catch (Exception e) {
                throwException(e, null);
            }
        }
        return result;
    }

    public static Boolean contains(Object xs, Object x) {
        var col = (Map<?, ?>) xs;
        return col.containsKey(x);
    }

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

    public static List<Object> filter(Object f, Object xs) {
        var func = (java.util.function.Function<Object, Object>) f;
        var col = (Collection<Object>) xs;
        var result = new ArrayList<Object>();
        for (Object x : col) {
            if (toBoolean(func.apply(x))) {
                result.add(x);
            }
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

    // region Lambda

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

    public static Object apply(Object f, Object args) {
        List<?> argList = (List<?>) args;
        int size = argList.size();
        try {
            switch (size) {
                case 0:
                    return ((Fn0) f).invoke();
                case 1:
                    return ((Fn1) f).invoke(argList.get(0));
                case 2:
                    return ((Fn2) f).invoke(argList.get(0), argList.get(1));
                case 3:
                    return ((Fn3) f).invoke(argList.get(0), argList.get(1), argList.get(2));
                case 4:
                    return ((Fn4) f).invoke(argList.get(0), argList.get(1), argList.get(2), argList.get(3));
                default:
                    throw new RuntimeException("apply: too many arguments (" + size + ")");
            }
        } catch (Exception e) {
            return throwSilent(e);
        }
    }

    // endregion
}
|}

let js_runtime = {|
export const hash_map_from = (xs) => {
    const result = {};
    for (let i = 0; i < xs.length; i += 2) {
        result[xs[i]] = xs[i + 1];
    }
    return result;
};

export const debug_assert = (a, b) => {
    return JSON.stringify(a) === JSON.stringify(b);
}

export const re_find = (p, i) => {
    const result = p.exec(i);
    if (result == null) {
        return null;
    }
    return result[0];
}

export const swap_BANG_ = (atom, f) => {
    const result = atom[0];
    atom[0] = f(result);
    return result;
}

export const _PLUS_ = (...xs) => {
    return xs.reduce((a, b) => a + b);
}

export const _MINUS_ = (a, b) => {
    return a - b
}

export const inc = (a) => {
    return a + 1;
}

export const update = (m, k, f) => {
    return { ...m, [k]: f(m[k]) };
}
|}

let java_runtime2 = {|
package y2k;

@SuppressWarnings("unchecked")
public class prelude_java {
public static final Object __namespace;
static {
__namespace="prelude_java";
};
public static /* final */ Object inc;
static {
inc=y2k.RT.fn((p__3)->{
int x=(int)((int)p__3);
return (x + 1);
});
};
public static /* final */ Object fixme;
static {
fixme=y2k.RT.fn((loc,xs)->{

return java.util.Objects.requireNonNull(
null,
String.format(
"%s%s%s",
loc,
" ",
xs));
});
};
public static /* final */ Object get;
static {
get=y2k.RT.fn((xs,i)->{
Object p__2;
if (y2k.RT.toBoolean(
(xs instanceof java.util.Map))) {
p__2=((java.util.Map)xs).get(i);
} else {
Object p__1;
if (y2k.RT.toBoolean(
(xs instanceof java.util.List))) {
p__1=((java.util.List)xs).get(((int)i));
} else {
p__1=y2k.RT.invoke(
y2k.prelude_java.fixme,
"prelude/data/prelude_java.clj:94:9",
java.util.Arrays.asList(
"Unsupported source: ",
String.format(
"%s",
xs),
", key: ",
String.format(
"%s",
i)));
};
p__2=p__1;
};
return p__2;
});
};
public static /* final */ Object update;
static {
update=y2k.RT.fn((m,k,f)->{

return y2k.RT.assoc(
m,
k,
y2k.RT.invoke(
f,
y2k.RT.invoke(
y2k.prelude_java.get,
m,
k)));
});
};
}

|}

let java_runtime2_v2 = {|
package y2k;

@SuppressWarnings("unchecked")
public class prelude_java_v2 {
public static final Object __namespace = "prelude_java_v2";
public static Object inc(Object p__3) throws Exception {
int x=(int)((int)p__3);
return (x + 1);
};
public static Object fixme(Object loc, Object xs) throws Exception {

return java.util.Objects.requireNonNull(
null,
String.format(
"%s%s%s",
loc,
" ",
xs));
};
public static Object get(Object xs, Object i) throws Exception {
Object p__2;
if (y2k.RT.toBoolean(
(xs instanceof java.util.Map))) {
p__2=((java.util.Map)xs).get(i);
} else {
Object p__1;
if (y2k.RT.toBoolean(
(xs instanceof java.util.List))) {
p__1=((java.util.List)xs).get(((int)i));
} else {
p__1=y2k.prelude_java_v2.fixme(
"prelude/data/prelude_java_v2.clj:91:9",
java.util.Arrays.asList(
"Unsupported source: ",
String.format(
"%s",
xs),
", key: ",
String.format(
"%s",
i)));
};
p__2=p__1;
};
return p__2;
};
public static Object update(Object m, Object k, Object f) throws Exception {

return y2k.RT.assoc(
m,
k,
y2k.RT.invoke(
f,
y2k.prelude_java_v2.get(
m,
k)));
};
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

(defn macro_update [m k f]
  (list
   'y2k.RT.invoke
   'y2k.prelude_java.update
   m k f))

(defn macro_assert [a b]
  (list '= a b))

(defn macro_parse-int [s]
  (list 'Integer/parseInt
        (list 'cast 'String s)))

(defn macro_subs [s sp ep]
  (list '.substring
        (list 'cast 'String s)
        (list 'cast 'int sp)
        (list 'cast 'int ep)))

(defn macro_string/join [sep xs]
  (list 'String/join
        (list 'cast 'String sep)
        (list 'cast 'java.util.Collection xs)))

(defn macro_string/split [s sep]
  (list 'vec
        (list '.split
              (list 'cast 'String s)
              (list 'cast 'String sep))))

(defn macro_string/starts-with? [s prefix]
  (list '.startsWith
        (list 'cast 'String s)
        (list 'cast 'String prefix)))

(defn macro_clojure.string/ends-with? [s suffix]
  (list '.endsWith
        (list 'cast 'String s)
        (list 'cast 'String suffix)))

(defn macro_clojure.string/replace [s match replacement]
  (list '.replace
        (list 'cast 'String s)
        (list 'cast 'String match)
        (list 'cast 'String replacement)))

(defn macro_declare [x] (list 'do))

(defn macro_boolean [x]
  (list 'y2k.RT.toBoolean x))

(defn macro_unixtime []
  (list '/ (list '.getTime (list 'java.util.Date.)) 1000.0))

(defn list [& xs] xs)

(defn macro_conj [xs x]
  (list 'y2k.RT.conj xs x))

(defn macro_map [f xs]
  (list 'y2k.RT.map f xs))

(defn macro_filter [f xs]
  (list 'y2k.RT.filter f xs))

(defn macro_hash-map-from [xs]
  (list 'y2k.RT.hash_map_from xs))

(defn macro_eprintln [& xs]
  (concat (list 'y2k.RT.eprintln) xs))

(defn macro_inc [x]
  (list 'y2k.RT.invoke
        'y2k.prelude_java.inc
        x))

(defn macro_FIXME [& xs]
  (list 'y2k.RT.invoke
        'y2k.prelude_java.fixme
        '__LOC__
        (concat (list 'vector) xs)))

;; (defn macro_FIXME [& xs]
;;   (list 'java.util.Objects.requireNonNull
;;         nil
;;         (list 'string/join
;;               " "
;;               (concat (list 'vector) xs))))

(defn macro_not= [x y]
  (list 'not (list '= x y)))

(defn macro_rest [xs]
  (let [xs_var (gensym)]
    (list
     'do
     (list
      'let xs_var (list 'cast 'java.util.List xs))
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

(defn macro_last [xs]
  (list 'get xs (list '- (list 'cast 'int (list 'count xs)) 1)))

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
    (list 'let (list 'vector var x)
          (list '.set
                (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
                var)
          var)))

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
   (list 'y2k.RT.hash_map)
   xs))

(defn macro_count [xs]
  (let* vxs (gensym))
  (list
   'do
   (list 'let vxs (list 'cast 'Object xs))
   (list
    'if (list 'instance? 'java.util.Map vxs)
    (list '. (list 'cast 'java.util.Map vxs) 'size)
    (list
     'if (list 'string? vxs)
     (list '.length (list 'cast 'String vxs))
     (list '. (list 'cast 'java.util.Collection vxs) 'size)))))

(defn macro_get [xs k]
  (list 'y2k.RT.invoke 'y2k.prelude_java.get xs k))

(defn macro_str [& xs]
  (concat
   (list
    'String.format
    (reduce (fn* [acc x] (str acc "%s")) "" xs))
   xs))

(defn macro_string? [x]
  (list 'instance? 'String x))

(defn macro_boolean? [x]
  (list 'instance? 'Boolean (list 'cast 'Object x)))

(defn macro_println [& xs]
  (list 'do
        (list 'System.out.println
              (concat (list 'str) xs))
        (list 'y2k.RT.nop)))

(defn macro_eprintln [& xs]
  (list 'do
        (list 'System.err.println
              (concat (list 'str) xs))
        (list 'y2k.RT.nop)))

;; Regex

(defn macro_re-pattern [x]
  (list 'java.util.regex.Pattern.compile x))

(defn macro_re-find [p i]
  (list 'y2k.RT.re_find p i))

;; Collections

(defn macro_vec [xs]
  (list 'y2k.RT.vec xs))

(defn macro_list [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))

(defn macro_vector [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))

(defn macro_reduce [f init xs]
  (list 'y2k.RT.reduce f init xs))

(defn macro_second [xs]
  (list 'get xs 1))

(defn macro_vector? [xs]
  (list 'instance? 'java.util.List xs))

(defn macro_contains? [m k]
  (list 'y2k.RT.contains m k))

(defn macro_shuffle [seed xs]
  (list 'y2k.RT.shuffle seed xs))

(defn macro_take [n xs]
  (list 'y2k.RT.take n xs))

|}

let prelude_java_v2_macro = {|
;; Shared definitions for all targets

;; (def false 0)
;; (def hash-map 0)
;; (def true 0)
;; (def vector 0)

;; (defmacro comment [& args] 'nil)
;; (defmacro first [xs] (list 'get xs 0))
;; (defmacro not= [a b] (list 'not (list '= a b)))

;; Specific target prelude

;; Prelude macros for java_v2 backend
;; Key difference: uses direct static method calls instead of y2k.RT.invoke

(defn macro_update [m k f]
  (list 'y2k.prelude_java_v2.update m k f))

(defn macro_assert [a b]
  (list '= a b))

(defn macro_parse-int [s]
  (list 'Integer/parseInt
        (list 'cast 'String s)))

(defn macro_subs [s sp ep]
  (list '.substring
        (list 'cast 'String s)
        (list 'cast 'int sp)
        (list 'cast 'int ep)))

(defn macro_string/join [sep xs]
  (list 'String/join
        (list 'cast 'String sep)
        (list 'cast 'java.util.Collection xs)))

(defn macro_string/split [s sep]
  (list 'vec
        (list '.split
              (list 'cast 'String s)
              (list 'cast 'String sep))))

(defn macro_string/starts-with? [s prefix]
  (list '.startsWith
        (list 'cast 'String s)
        (list 'cast 'String prefix)))

(defn macro_clojure.string/ends-with? [s suffix]
  (list '.endsWith
        (list 'cast 'String s)
        (list 'cast 'String suffix)))

(defn macro_clojure.string/replace [s match replacement]
  (list '.replace
        (list 'cast 'String s)
        (list 'cast 'String match)
        (list 'cast 'String replacement)))

(defn macro_declare [x] (list 'do))

(defn macro_boolean [x]
  (list 'y2k.RT.toBoolean x))

(defn macro_unixtime []
  (list '/ (list '.getTime (list 'java.util.Date.)) 1000.0))

(defn list [& xs] xs)

(defn macro_conj [xs x]
  (list 'y2k.RT.conj xs x))

(defn macro_map [f xs]
  (list 'y2k.RT.map f xs))

(defn macro_filter [f xs]
  (list 'y2k.RT.filter f xs))

(defn macro_hash-map-from [xs]
  (list 'y2k.RT.hash_map_from xs))

(defn macro_eprintln [& xs]
  (concat (list 'y2k.RT.eprintln) xs))

(defn macro_inc [x]
  (list 'y2k.prelude_java_v2.inc x))

(defn macro_FIXME [& xs]
  (list 'y2k.prelude_java_v2.fixme
        '__LOC__
        (concat (list 'vector) xs)))

(defn macro_not= [x y]
  (list 'not (list '= x y)))

(defn macro_rest [xs]
  (let [xs_var (gensym)]
    (list
     'do
     (list
      'let xs_var (list 'cast 'java.util.List xs))
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

(defn macro_last [xs]
  (list 'get xs (list '- (list 'cast 'int (list 'count xs)) 1)))

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
    (list 'let (list 'vector var x)
          (list '.set
                (list 'cast 'java.util.concurrent.atomic.AtomicReference a)
                var)
          var)))

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
   (list 'y2k.RT.hash_map)
   xs))

(defn macro_count [xs]
  (let* vxs (gensym))
  (list
   'do
   (list 'let vxs (list 'cast 'Object xs))
   (list
    'if (list 'instance? 'java.util.Map vxs)
    (list '. (list 'cast 'java.util.Map vxs) 'size)
    (list
     'if (list 'string? vxs)
     (list '.length (list 'cast 'String vxs))
     (list '. (list 'cast 'java.util.Collection vxs) 'size)))))

(defn macro_get [xs k]
  (list 'y2k.prelude_java_v2.get xs k))

(defn macro_str [& xs]
  (concat
   (list
    'String.format
    (reduce (fn* [acc x] (str acc "%s")) "" xs))
   xs))

(defn macro_string? [x]
  (list 'instance? 'String x))

(defn macro_boolean? [x]
  (list 'instance? 'Boolean (list 'cast 'Object x)))

(defn macro_println [& xs]
  (list 'do
        (list 'System.out.println
              (concat (list 'str) xs))
        (list 'y2k.RT.nop)))

(defn macro_eprintln [& xs]
  (list 'do
        (list 'System.err.println
              (concat (list 'str) xs))
        (list 'y2k.RT.nop)))

;; Regex

(defn macro_re-pattern [x]
  (list 'java.util.regex.Pattern.compile x))

(defn macro_re-find [p i]
  (list 'y2k.RT.re_find p i))

;; Collections

(defn macro_vec [xs]
  (list 'y2k.RT.vec xs))

(defn macro_list [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))

(defn macro_vector [& xs]
  (concat
   (list 'java.util.Arrays.asList)
   xs))

(defn macro_reduce [f init xs]
  (list 'y2k.RT.reduce f init xs))

(defn macro_second [xs]
  (list 'get xs 1))

(defn macro_vector? [xs]
  (list 'instance? 'java.util.List xs))

(defn macro_contains? [m k]
  (list 'y2k.RT.contains m k))

(defn macro_shuffle [seed xs]
  (list 'y2k.RT.shuffle seed xs))

(defn macro_take [n xs]
  (list 'y2k.RT.take n xs))

(defn macro_apply [f xs]
  (list 'y2k.RT.apply f xs))

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

(defn not [x] (if x false true))

(defn nil? [x] (= x nil))
(defn some? [x] (not (nil? x)))

(defn not= [a b] (not (= a b)))

(defn first [xs] (get xs 0))
(defn second [xs] (get xs 1))

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

(defn macro_update [m k f]
  (list 'prelude/update m k f))

(defn macro_inc [a]
  (list 'prelude/inc a))

(defn macro_* [a b]
  (list '__inline_op__ "*" a b))

(defn macro_mod [a b]
  (list '__inline_op__ "%" a b))

(defn macro_> [a b]
  (list '__inline_op__ ">" a b))

(defn macro_>= [a b]
  (list '__inline_op__ ">=" a b))

(defn macro_< [a b]
  (list '__inline_op__ "<" a b))

(defn macro_<= [a b]
  (list '__inline_op__ "<=" a b))

(defn macro_+ [& xs]
  (concat (list 'prelude/+) xs))

(defn macro_- [& xs]
  (concat (list 'prelude/_MINUS_) xs))

;;

(defn macro_hash-map-from [xs]
  (list 'prelude/hash_map_from xs))

(defn macro_assert [a b]
  (list 'prelude/debug_assert a b))

(defn macro_not= [x y]
  (list 'not (list '= x y)))

(defn macro_parse-int [s]
  (list 'parseInt s))

(defn macro_subs [s sp ep]
  (list '.substring s sp ep))

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

(defn macro_merge [m1 m2]
  (list 'Object.assign (list 'hash-map) m1 m2))

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

;; Strings

(defn macro_string/join [sep xs]
  (list '.join xs sep))

(defn macro_string/split [s sep]
  (list '.split s sep))

(defn macro_string/starts-with? [s prefix]
  (list '.startsWith s prefix))

(defn macro_clojure.string/ends-with? [s suffix]
  (list '.endsWith s suffix))

(defn macro_clojure.string/replace [s match replacement]
  (list '.replaceAll s match replacement))

;; Regex

(defn macro_re-pattern [x]
  (list 'RegExp. x))

;; (defn macro_re-find [p i]
;;   (list '.exec p i))

(defn macro_re-find [p i]
  (list 'prelude/re_find p i))

;; Collections

(defn macro_vec [x] x)

(defn macro_list [& xs]
  (concat
   (list 'vector)
   xs))

(defn macro_map [f xs]
  (list '.map xs f))

(defn macro_filter [f xs]
  (list '.filter xs f))

(defn macro_second [xs]
  (list 'get xs 1))

(defn macro_contains? [xs x]
  (list '.hasOwnProperty xs x))

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
  (list 'prelude/swap! a f))

(defn macro_reset! [a x]
  (list 'assoc! a 0 x))

(defn macro_atom [x]
  (list 'vector x))

(defn macro_deref [a]
  (list 'get a 0))
|}
