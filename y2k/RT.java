package y2k;

public class RT {
    interface Fn0 {
        public Object invoke() throws Exception;
    }

    interface Fn1 {
        public Object invoke(Object a) throws Exception;
    }

    public static Object fn(Fn0 f) {
        return f;
    }

    public static Object fn(Fn1 f) {
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
}