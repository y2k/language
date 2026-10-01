package y2k.language;

public final class language_runtime {
  public static Object re_pattern(Object... args) {
    // A lone null literal is passed by javac as a null varargs array.
    if (args == null) throw new RuntimeException("re-pattern expects a string");
    if (args.length != 1) throw new RuntimeException("re-pattern expects 1 argument");
    if (!(args[0] instanceof String pattern)) throw new RuntimeException("re-pattern expects a string");
    var normalized = new StringBuilder();
    boolean inClass = false;
    for (int i = 0; i < pattern.length(); i++) {
      char c = pattern.charAt(i);
      if (c == '\\' && i + 1 < pattern.length()) {
        normalized.append(c).append(pattern.charAt(++i));
      } else if (c == '$' && !inClass) {
        normalized.append("\\z");
      } else {
        normalized.append(c);
        if (c == '[') inClass = true;
        else if (c == ']') inClass = false;
      }
    }
    try {
      return java.util.regex.Pattern.compile(normalized.toString());
    } catch (java.util.regex.PatternSyntaxException error) {
      throw new RuntimeException("re-pattern: invalid pattern", error);
    }
  }

  public static Object re_find(Object... args) {
    if (args == null || args.length != 2) throw new RuntimeException("re-find expects 2 arguments");
    if (!(args[0] instanceof java.util.regex.Pattern regex)) throw new RuntimeException("re-find expects a regex");
    if (!(args[1] instanceof String text)) throw new RuntimeException("re-find expects a string text");
    var matcher = regex.matcher(text);
    return matcher.find() ? matcher.group() : null;
  }

  public static Object re_replace(Object... args) {
    if (args == null || args.length != 3) throw new RuntimeException("re-replace expects 3 arguments");
    if (!(args[1] instanceof java.util.regex.Pattern regex)) throw new RuntimeException("re-replace expects a regex");
    if (!(args[0] instanceof String text) || !(args[2] instanceof String replacement))
      throw new RuntimeException("re-replace expects string text and replacement");
    var matcher = regex.matcher(text);
    var result = new StringBuilder();
    int copied = 0;
    int search = 0;
    int nonemptyEnd = -1;
    while (matcher.find(search)) {
      int start = matcher.start();
      int end = matcher.end();
      // Re skips the empty match immediately following a nonempty one.
      if (start != end || start != nonemptyEnd) {
        result.append(text, copied, start).append(replacement);
        copied = end;
      }
      if (start == end) {
        if (end == text.length()) break;
        search = end + 1;
      } else {
        nonemptyEnd = end;
        search = end;
      }
    }
    return result.append(text, copied, text.length()).toString();
  }


  private static final class Atom {
    Object value;

    Atom(Object value) {
      this.value = value;
    }
  }

  public static Object atom(Object value) {
    return new Atom(value);
  }

  public static Object deref(Object reference) {
    if (!(reference instanceof Atom cell))
      throw new RuntimeException("deref expects one atom");
    return cell.value;
  }

  public static Object reset_BANG_(Object reference, Object value) {
    if (!(reference instanceof Atom cell))
      throw new RuntimeException("reset! expects an atom and a value");
    cell.value = value;
    return value;
  }

  public static Object swap_BANG_(Object reference, Object fn) throws Exception {
    if (!(reference instanceof Atom cell))
      throw new RuntimeException("swap! expects an atom and a function");
    // ponytail: sequential updates; synchronization needs a separate concurrency contract.
    Object value = call_fn(fn, cell.value);
    cell.value = value;
    return value;
  }

  @FunctionalInterface
  public interface Fn0 {
    Object call() throws Exception;
  }

  @FunctionalInterface
  public interface Fn1 {
    Object call(Object a) throws Exception;
  }

  @FunctionalInterface
  public interface Fn2 {
    Object call(Object a, Object b) throws Exception;
  }

  @FunctionalInterface
  public interface Fn3 {
    Object call(Object a, Object b, Object c) throws Exception;
  }

  @FunctionalInterface
  public interface Fn4 {
    Object call(Object a, Object b, Object c, Object d) throws Exception;
  }

  public static RuntimeException sneaky_throw(Throwable throwable) {
    language_runtime.<RuntimeException>sneaky_throw_unchecked(throwable);
    return null;
  }

  @SuppressWarnings("unchecked")
  private static <T extends Throwable> void sneaky_throw_unchecked(Throwable throwable) throws T {
    throw (T) throwable;
  }

  public static java.util.List<Object> list(Object... items) {
    return java.util.Arrays.asList(items);
  }

  public static Boolean vector_QMARK_(Object value) {
    return value instanceof java.util.List<?>;
  }

  public static Boolean _EQ_(Object left, Object right) {
    if (left instanceof java.util.regex.Pattern || right instanceof java.util.regex.Pattern) return false;
    return java.util.Objects.equals(left, right);
  }

  public static Boolean not_EQ_(Object left, Object right) {
    return !_EQ_(left, right);
  }

  public static Boolean _GT_(Object left, Object right) {
    return ((Number) left).intValue() > ((Number) right).intValue();
  }

  public static Boolean _LT_(Object left, Object right) {
    return ((Number) left).intValue() < ((Number) right).intValue();
  }

  public static Boolean _GT__EQ_(Object left, Object right) {
    return ((Number) left).intValue() >= ((Number) right).intValue();
  }

  public static Boolean _LT__EQ_(Object left, Object right) {
    return ((Number) left).intValue() <= ((Number) right).intValue();
  }

  public static java.util.List<Object> concat(Object... collections) {
    var result = new java.util.ArrayList<Object>();
    for (Object collection : collections) {
      if (!(collection instanceof java.util.List<?> items))
        throw new RuntimeException("concat expects lists");
      result.addAll(items);
    }
    return result;
  }

  public static java.util.LinkedHashMap<String, Object> hash_map(Object... items) {
    if (items.length % 2 != 0) {
      throw new RuntimeException("hash-map arguments must be key/value pairs");
    }
    var result = new java.util.LinkedHashMap<String, Object>();
    for (int i = 0; i < items.length; i += 2) {
      result.put(value_text(items[i]), items[i + 1]);
    }
    return result;
  }

  public static Object get(Object collection, Object key) {
    if (collection == null) return null;
    if (collection instanceof java.util.List<?> items && key instanceof Number index) {
      int i = index.intValue();
      return i >= 0 && i < items.size() ? items.get(i) : null;
    }
    if (collection instanceof java.util.Map<?, ?> items)
      return items.get(value_text(key));
    throw new RuntimeException("get expects a hash-map/list and a key/index");
  }

  public static Object get_in(Object collection, Object keys) {
    if (!(keys instanceof java.util.List<?> path))
      throw new RuntimeException("get-in expects a collection and a vector path");
    for (Object key : path)
      collection = get(collection, key);
    return collection;
  }

  public static boolean truthy(Object value) {
    return !(value == null || Boolean.FALSE.equals(value));
  }

  public static Boolean not(Object value) {
    return !truthy(value);
  }

  public static Object print_result(Object value) {
    System.out.println(value_text(value));
    return null;
  }

  public static Object println(Object... items) {
    System.out.println(join_str(items, " "));
    return null;
  }

  public static Object eprintln(Object... items) {
    System.err.println(join_str(items, " "));
    return null;
  }

  public static Object missing(Object... items) {
    throw new RuntimeException("missing");
  }

  public static String str(Object... items) {
    return join_str(items, "");
  }

  private static boolean has_floating(Object[] items) {
    for (Object item : items)
      if (item instanceof Double || item instanceof Float) return true;
    return false;
  }

  private static Number normalize_number(double value) {
    if (value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE && value == Math.rint(value))
      return (int) value;
    return value;
  }

  public static Number _PLUS_(Object... items) {
    if (has_floating(items)) {
      double result = 0;
      for (Object item : items)
        result += ((Number) item).doubleValue();
      return normalize_number(result);
    }
    int result = 0;
    for (Object item : items)
      result += ((Number) item).intValue();
    return result;
  }

  public static Number _MINUS_(Object... items) {
    if (items.length == 0)
      throw new RuntimeException("- expects at least one number");
    if (has_floating(items)) {
      double result = ((Number) items[0]).doubleValue();
      for (int i = 1; i < items.length; i++)
        result -= ((Number) items[i]).doubleValue();
      return normalize_number(result);
    }
    int result = ((Number) items[0]).intValue();
    for (int i = 1; i < items.length; i++)
      result -= ((Number) items[i]).intValue();
    return result;
  }

  public static Number _STAR_(Object... items) {
    if (has_floating(items)) {
      double result = 1;
      for (Object item : items)
        result *= ((Number) item).doubleValue();
      return normalize_number(result);
    }
    int result = 1;
    for (Object item : items)
      result *= ((Number) item).intValue();
    return result;
  }

  public static Integer _SLASH_(Object... items) {
    if (items.length == 0)
      throw new RuntimeException("/ expects at least one number");
    int result = ((Number) items[0]).intValue();
    for (int i = 1; i < items.length; i++)
      result /= ((Number) items[i]).intValue();
    return result;
  }

  public static Integer count(Object collection) {
    if (collection instanceof java.util.List<?> list)
      return list.size();
    if (collection instanceof java.util.Map<?, ?> map)
      return map.size();
    throw new RuntimeException("count expects one collection");
  }

  public static java.util.List<Object> map(Object fn, Object collection) throws Exception {
    if (!(collection instanceof java.util.List<?> items)) {
      throw new RuntimeException("map expects a function and a list");
    }
    var result = new java.util.ArrayList<Object>();
    for (Object item : items)
      result.add(call_fn(fn, item));
    return result;
  }

  public static java.util.List<Object> drop(Object count, Object collection) {
    var items = (java.util.List<?>) collection;
    int start = Math.min(Math.max(((Number) count).intValue(), 0), items.size());
    return new java.util.ArrayList<Object>(items.subList(start, items.size()));
  }

  static java.util.List<?> reduce_items(Object collection) {
    if (collection instanceof java.util.List<?> items)
      return items;
    if (collection instanceof java.util.Map<?, ?> map) {
      var items = new java.util.ArrayList<java.util.List<Object>>();
      for (var entry : map.entrySet())
        items.add(java.util.Arrays.asList(entry.getKey(), entry.getValue()));
      return items;
    }
    throw new RuntimeException("reduce expects a list or hash-map");
  }

  public static Object reduce(Object fn, Object collection) throws Exception {
    var items = reduce_items(collection);
    if (items.isEmpty()) {
      throw new RuntimeException("reduce expects a non-empty list");
    }
    Object acc = items.get(0);
    for (int i = 1; i < items.size(); i++)
      acc = call_fn(fn, acc, items.get(i));
    return acc;
  }

  public static Object reduce(Object fn, Object init, Object collection) throws Exception {
    var items = reduce_items(collection);
    Object acc = init;
    for (Object item : items)
      acc = call_fn(fn, acc, item);
    return acc;
  }

  static Object call_fn(Object fn, Object... args) throws Exception {
    if (fn instanceof Fn0 callable) {
      expect_args("function", args, 0);
      return callable.call();
    }
    if (fn instanceof Fn1 callable) {
      expect_args("function", args, 1);
      return callable.call(args[0]);
    }
    if (fn instanceof Fn2 callable) {
      expect_args("function", args, 2);
      return callable.call(args[0], args[1]);
    }
    if (fn instanceof Fn3 callable) {
      expect_args("function", args, 3);
      return callable.call(args[0], args[1], args[2]);
    }
    if (fn instanceof Fn4 callable) {
      expect_args("function", args, 4);
      return callable.call(args[0], args[1], args[2], args[3]);
    }
    throw new RuntimeException("value is not a function");
  }

  static void expect_args(String name, Object[] args, int count) {
    if (args.length != count) {
      throw new RuntimeException(name + " expects " + count + " arguments");
    }
  }

  static String value_text(Object value) {
    if (value instanceof java.util.regex.Pattern)
      return "#<regex>";
    if (value == null)
      return "nil";
    if (value instanceof Atom)
      return "#<atom>";
    if (value instanceof java.util.List<?> list) {
      var items = new java.util.ArrayList<String>();
      for (Object item : list)
        items.add(value_text(item));
      return "(" + String.join(" ", items) + ")";
    }
    if (value instanceof java.util.Map<?, ?> map) {
      var items = new java.util.ArrayList<String>();
      for (var entry : map.entrySet()) {
        items.add(entry.getKey() + " " + value_text(entry.getValue()));
      }
      return "{" + String.join(" ", items) + "}";
    }
    if (value instanceof Fn0 || value instanceof Fn1 || value instanceof Fn2 || value instanceof Fn3
        || value instanceof Fn4)
      return "#<function>";
    return String.valueOf(value);
  }

  static String str_text(Object value) {
    return value_text(value);
  }

  static String join_str(Object[] items, String separator) {
    var parts = new java.util.ArrayList<String>();
    for (Object item : items)
      parts.add(str_text(item));
    return String.join(separator, parts);
  }
}
