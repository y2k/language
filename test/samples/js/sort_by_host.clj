;; true|true|true|true
(defn test []
  (raw-code "const a = {key: '2026-10-14'}, b = {key: '2026-10-13'}, c = {key: '2026-10-14'};
const items = [a, b, c], trace = [];
let unary = true;
const sorted = sort_by(function(item) {
  unary = unary && arguments.length === 1;
  trace.push(item);
  return item.key;
}, items);
const identity = sorted.length === 3 && sorted[0] === b && sorted[1] === a && sorted[2] === c;
const unchanged = items.length === 3 && items[0] === a && items[1] === b && items[2] === c;
const calls = unary && trace.length === 3 && trace[0] === a && trace[1] === b && trace[2] === c;
const failure = new Error('stop'), visited = [];
let caught = null;
try {
  sort_by(item => { visited.push(item); if (item === b) throw failure; return item.key; }, items);
} catch (error) { caught = error; }
const stopped = caught === failure && visited.length === 2 && visited[0] === a && visited[1] === b;")
  (str identity "|" unchanged "|" calls "|" stopped))
