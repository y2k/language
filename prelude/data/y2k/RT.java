package y2k;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

@SuppressWarnings("unchecked")
public class RT {

  private static AtomicLong gensym_id = new AtomicLong(0);

  public static String gensym() {
    return "G__" + gensym_id.getAndIncrement();
  }

  public static Object recover(Object f, Object fe) {
    try {
      return ((Fn) f).invoke();
    } catch (Exception e) {
      return ((Fn) fe).invoke(e);
    }
  }

  public static Object run_(Object farg, Object xs) {
    var f = (Function<Object, Object>) farg;
    var col = (List<Object>) xs;
    for (Object x : col) {
      f.apply(x);
    }
    return null;
  }

  public static Object reduce(Object f, Object def, Object xs) {
    var col = (Collection<Object>) xs;
    var func = (Fn<Object, Object>) f;
    var acc = def;
    for (Object x : col) {
      acc = func.invoke(acc, x);
    }
    return acc;
  }

  public static boolean contains(Object xs, Object x) {
    if (xs instanceof Map) {
      var map = (Map<Object, Object>) xs;
      return map.containsKey(x);
    }
    if (xs instanceof Collection) {
      var col = (Collection<Object>) xs;
      return col.contains(x);
    }
    throw new RuntimeException("Unsupported source: " + xs);
  }

  public static List<Object> map(Object f, Object xs) {
    var col = (Collection<Object>) xs;
    var result = new ArrayList<Object>(col.size());
    var func = (Function<Object, Object>) f;
    for (Object x : col) {
      result.add(func.apply(x));
    }
    return result;
  }

  public static List<Object> rest(Object xs) {
    var col = (List<Object>) xs;
    return col.subList(1, col.size());
  }

  public static boolean equals(Object a, Object b) {
    return java.util.Objects.equals(a, b);
  }

  public static Map<Object, Object> hash_map(Object... args) {
    var result = new HashMap<Object, Object>();
    for (int i = 0; i < args.length; i += 2) {
      result.put(args[i], args[i + 1]);
    }
    return result;
  }

  public static int count(Object xs) {
    return ((List<Object>) xs).size();
  }

  public static <T, R> Function<T, R> function(Function<T, R> f) {
    return f;
  }

  public static Map<Object, Object> merge(Object as, Object bs) {
    var a = (Map<Object, Object>) as;
    var b = (Map<Object, Object>) bs;
    var result = new HashMap<>(a);
    result.putAll(b);
    return result;
  }

  public static List<Object> concat(Object as, Object bs) {
    var a = (List<Object>) as;
    var b = (List<Object>) bs;
    var result = new ArrayList<>(a);
    result.addAll(b);
    return result;
  }

  public static Map<Object, Object> assoc(Object xs, Object k, Object v) {
    var col = (Map<Object, Object>) xs;
    var result = new HashMap<>(col);
    result.put(k, v);
    return result;
  }

  public static boolean empty(Object xs) {
    return ((List<Object>) xs).isEmpty();
  }

  public static List<Object> conj(Object xs, Object x) {
    var col = (Collection<Object>) xs;
    var result = new ArrayList<>(col);
    result.add(x);
    return result;
  }

  public static Object[] into_array(Object xs) {
    var col = (List<Object>) xs;
    var result = (Object[]) Array.newInstance(col.get(0).getClass(), col.size());
    return col.toArray(result);
  }

  public static Object[] into_array(Class<?> cls, Object xs) {
    var col = (List<Object>) xs;
    var result = (Object[]) Array.newInstance(cls, col.size());
    return col.toArray(result);
  }

  public static String str(Object... args) {
    if (args.length == 1) {
      return Objects.toString(args[0]);
    }
    StringBuilder sb = new StringBuilder();
    for (Object arg : args) {
      sb.append(arg);
    }
    return sb.toString();
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

  private static <T extends Throwable> void throwException(
      Throwable exception,
      Object dummy) throws T {
    throw (T) exception;
  }

  public static <T> T throw_(Object exception) {
    throwException((Throwable) exception, null);
    return null;
  }

  public static <T> T try_(Object f) {
    try {
      return (T) ((Fn) f).invoke();
    } catch (Exception e) {
      RT.throwException(e, null);
      return null;
    }
  }

  public static Object println(Object... xs) {
    for (Object x : xs) {
      System.out.print(x);
      System.out.print(" ");
    }
    System.out.println();
    return null;
  }

  public static Runnable runnable(Supplier<Object> f) {
    return f::get;
  }

  public static String unescape(Object input) {
    var str = input.toString();
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < str.length(); i++) {
      char c = str.charAt(i);
      if (c == '\\' && i + 1 < str.length()) {
        char next = str.charAt(i + 1);
        if (next == 'n') {
          sb.append('\n');
          i++;
        } else if (next == '\\') {
          sb.append('\\');
          i++;
        } else if (next == '\"') {
          sb.append('\"');
          i++;
        } else {
          sb.append(c);
        }
      } else {
        sb.append(c);
      }
    }
    return sb.toString();
  }

  public static Object invoke(Object f, Object... args) {
    var fn = (Fn) f;
    if (args.length == 0)
      return fn.invoke();
    if (args.length == 1)
      return fn.invoke(args[0]);
    if (args.length == 2)
      return fn.invoke(args[0], args[1]);
    if (args.length == 3)
      return fn.invoke(args[0], args[1], args[2]);
    if (args.length == 4)
      return fn.invoke(args[0], args[1], args[2], args[3]);
    throw new RuntimeException("Unsupported arity: " + args.length);
  }

  public interface Fn0 {
    Object invoke() throws Exception;
  }

  public interface Fn1 {
    Object invoke(Object a1) throws Exception;
  }

  public interface Fn2 {
    Object invoke(Object a1, Object a2) throws Exception;
  }

  public interface Fn3 {
    Object invoke(Object a1, Object a2, Object a3) throws Exception;
  }

  public interface Fn4 {
    Object invoke(Object a1, Object a2, Object a3, Object a4) throws Exception;
  }

  public static class Fn<TI, TR> implements Runnable, Callable<TR>, Consumer<TI>, UnaryOperator<Object>,
      Supplier<TR>, Fn0, Fn1, Fn2, Fn3, Fn4 {
    public Object invoke() {
      return invoke(null);
    }

    public Object invoke(Object a1) {
      return invoke(a1, null);
    }

    public Object invoke(Object a1, Object a2) {
      return invoke(a1, a2, null);
    }

    public Object invoke(Object a1, Object a2, Object a3) {
      return invoke(a1, a2, a3, null);
    }

    public Object invoke(Object a1, Object a2, Object a3, Object a4) {
      throw new RuntimeException("Not implemented");
    }

    public final void run() {
      invoke();
    }

    public final TR call() {
      return (TR) invoke();
    }

    @Override
    public final void accept(TI t) {
      invoke(t);
    }

    @Override
    public final Object apply(Object t) {
      return invoke(t);
    }

    @Override
    public final TR get() {
      return (TR) invoke();
    }
  }

  public static Fn fn(Fn0 f) {
    return new Fn() {
      @Override
      public Object invoke() {
        try {
          return f.invoke();
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn1 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1) {
        try {
          return f.invoke(a1);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn2 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1, Object a2) {
        try {
          return f.invoke(a1, a2);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn3 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1, Object a2, Object a3) {
        try {
          return f.invoke(a1, a2, a3);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }

  public static Fn fn(Fn4 f) {
    return new Fn() {
      @Override
      public Object invoke(Object a1, Object a2, Object a3, Object a4) {
        try {
          return f.invoke(a1, a2, a3, a4);
        } catch (Exception e) {
          return throw_(e);
        }
      }
    };
  }
}