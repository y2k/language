;; true|true|true|true|true
(defn test []
  (raw-code "const a = {id: 1}, b = {id: 2}, c = {id: 3};
const items = [a, b, c];
const selected = filter(item => item !== b, items);
const kept = keep(item => item === b ? undefined : item, items);
const identity = selected.length === 2 && selected[0] === a && selected[1] === c &&
  kept.length === 2 && kept[0] === a && kept[1] === c;
const unchanged = items.length === 3 && items[0] === a && items[1] === b && items[2] === c;
const values = keep(item => item, [null, undefined, false, 0, '']);
const missing = values.length === 3 && values[0] === false && values[1] === 0 && values[2] === '' &&
  filter(() => undefined, [1]).length === 0;
let unary = true, stopped = true;
for (const operation of [filter, keep]) {
  const trace = [], failure = new Error('stop');
  let caught = null;
  try {
    operation(function(item) {
      unary = unary && arguments.length === 1;
      trace.push(item);
      if (item === 2) throw failure;
      return item;
    }, [1, 2, 3]);
  } catch (error) { caught = error; }
  stopped = stopped && caught === failure && trace.length === 2 && trace[0] === 1 && trace[1] === 2;
}")
  (str identity "|" unchanged "|" missing "|" unary "|" stopped))
