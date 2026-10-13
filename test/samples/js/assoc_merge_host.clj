;; true true true true true

(defn test []
  (let [item (JSON/parse "{\"reason\":\"selected\",\"status\":\"pending\"}")
        result (JSON/parse "{\"status\":\"ready\",\"enabled\":false}")
        event (assoc (merge item result) :selection_reason (:reason item))]
    (raw-code "const source = JSON.parse('{\"__proto__\": 1, \"constructor\": 2}');
const inherited = Object.create({hidden: 3});
inherited.own = 4;
const combined = merge(source, inherited);
const updated = assoc(source, '__proto__', 5);
const constructor = assoc(source, 'constructor', 6);
const special = Object.getPrototypeOf(combined) === null &&
  Object.getPrototypeOf(updated) === null && combined.__proto__ === 1 &&
  combined.constructor === 2 && updated.__proto__ === 5 && constructor.constructor === 6 &&
  combined.own === 4 && !Object.hasOwn(combined, 'hidden') &&
  source.__proto__ === 1 && source.constructor === 2;
const empty = hash_map();
const fresh = merge(empty, empty) !== empty && assoc(empty, 'x', 1) !== empty;
let errors = true;
const invalid = [
  () => assoc(), () => assoc(empty), () => assoc(empty, 'x'),
  () => assoc(empty, 'x', 1, 'y', 2),
  () => merge(), () => merge(empty), () => merge(empty, empty, empty)
];
for (const value of [null, [], 42, false, 'text']) {
  invalid.push(() => assoc(value, 'x', 1), () => merge(value, empty), () => merge(empty, value));
}
for (const action of invalid) {
  let rejected = false;
  try { action(); } catch (error) { rejected = error instanceof Error; }
  errors = errors && rejected;
}")
    (str (and (= (:status event) "ready") (= (:enabled event) false)
              (= (:selection_reason event) "selected")) " "
         (and (= (:status item) "pending") (= (count item) 2) (= (count result) 2)) " "
         special " " fresh " " errors)))
