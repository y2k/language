package y2k;

public class RT {

    public static Object assoc(Object xs, Object k, Object v) {
        var col = (java.util.Map<Object, Object>) xs;
        var result = new java.util.HashMap<>(col);
        result.put(k, v);
        return result;
    }

    public interface Fn0 {
        public Object invoke() throws Exception;
    }

    public interface Fn1 extends java.util.function.UnaryOperator<Object> {
        public Object invoke(Object a) throws Exception;

        public default Object apply(Object a) {
            try {
                return invoke(a);
            } catch (Exception e) {
                throw new RuntimeException(e);
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
        return f;
    }

    public static Object fn(Fn1 f) {
        return f;
    }

    public static Object fn(Fn2 f) {
        return f;
    }

    public static Object fn(Fn3 f) {
        return f;
    }

    public static Object fn(Fn4 f) {
        return f;
    }

    public static Object invoke(Object f) {
        try {
            return ((Fn0) f).invoke();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static Object invoke(Object f, Object a) {
        try {
            return ((Fn1) f).invoke(a);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static Object invoke(Object f, Object a, Object b) {
        try {
            return ((Fn2) f).invoke(a, b);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static Object invoke(Object f, Object a, Object b, Object c) {
        try {
            return ((Fn3) f).invoke(a, b, c);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static Object invoke(Object f, Object a, Object b, Object c, Object d) {
        try {
            return ((Fn4) f).invoke(a, b, c, d);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}