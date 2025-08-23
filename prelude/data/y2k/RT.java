package y2k;

import java.util.*;

public class RT {

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