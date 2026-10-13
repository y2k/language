;; true true true

(ns checks.effects.batch)

(defn batch [effects]
  (fn [ctx]
    (reduce (fn [acc fx]
              (.then acc
                     (fn [result]
                       (.then (fx ctx)
                              (fn [x] (conj result x))))))
            (Promise/resolve [])
            effects)))

(raw-code "
const events = [];
const ctx = {};
const first = {id: 1};
const second = {id: 2};
const results = await batch([
  context => {
    if (context !== ctx) throw new Error('first context');
    events.push('start1');
    return Promise.resolve().then(() => {
      events.push('finish1');
      return first;
    });
  },
  context => {
    if (context !== ctx) throw new Error('second context');
    events.push('start2');
    return Promise.resolve(second);
  }
])(ctx);
const ordered = events.join(',') === 'start1,finish1,start2'
  && results.length === 2 && results[0] === first && results[1] === second;
const empty = await batch([])(ctx);
const emptyOK = Array.isArray(empty) && empty.length === 0 && events.length === 3;
const source = [first];
const appended = conj(source, second);
const identityOK = appended !== source && source.length === 1 && source[0] === first
  && appended.length === 2 && appended[0] === first && appended[1] === second;
")

(defn test [] (str ordered " " emptyOK " " identityOK))
