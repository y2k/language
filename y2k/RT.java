package y2k;

public class RT {
    interface Fn0 {
        public Object invoke() throws Exception;
    }

    interface Fn1 extends java.util.function.UnaryOperator<Object> {
        public Object invoke(Object a) throws Exception;

        public default Object apply(Object a) {
            try {
                return invoke(a);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    interface Fn2 {
        public Object invoke(Object a, Object b) throws Exception;
    }

    interface Fn3 {
        public Object invoke(Object a, Object b, Object c) throws Exception;
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
}