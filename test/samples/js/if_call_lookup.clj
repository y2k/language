;; true|true|true|true|true|true|true|true
(raw-code "let hostTrace = '';
const hostFailure = new Error('lookup');
const obj = { tag: 'T', get method() {
  hostTrace += 'lookup,';
  return function (x) { hostTrace += 'call'; return this.tag + 'old' + x; };
}};
function replaceMethod() {
  hostTrace += 'condition,';
  Object.defineProperty(obj, 'method', {value: () => 'new', configurable: true});
  return true;
}
const qualified = {tag: 'Q', method(x) {return this.tag + 'old' + x;}};
function replaceQualified() {qualified.method = () => 'new'; return true;}
let hostFn = x => 'old';
let Host = class {constructor(x) {this.tag = 'old';}};
function replaceConstructor() {Host = class {constructor(x) {this.tag = 'new';}}; return true;}
const bad = {get method() {throw hostFailure;}};
const nonCallable = {method: 42};
function late() {hostTrace += 'late'; return true;}
function lookupCheck() {hostTrace = ''; try {badLookup(); return false;} catch(e) {return e === hostFailure && hostTrace === '';}}
function callableCheck() {hostTrace = ''; try {badCall(); return false;} catch(e) {return e instanceof TypeError && hostTrace === 'late';}}
const maker = { build: function (x) {this.tag = 'old';} };
function replaceQualifiedConstructor() {maker.build = function (x) {this.tag = 'new';}; return true;}")
(defn badLookup [] (. bad method (if (late) 1 0)))
(defn badCall [] (. nonCallable method (if (late) 1 0)))
(export-default "example" (if true nil false))
(defn test []
  (let [method (. obj method (if (replaceMethod) 1 0))
        order (= hostTrace "lookup,condition,call")
        qualified-result (qualified/method (if (replaceQualified) 1 0))
        named (hostFn (if true (do (set! hostFn (fn [x] "new")) 1) 0))
        constructed (new Host (if (replaceConstructor) 1 0))
        qualified-constructed (new maker/build (if (replaceQualifiedConstructor) 1 0))]
    (str (= method "Told1") "|" order "|" (= qualified-result "Qold1") "|"
         (= named "old") "|" (= (get constructed :tag) "old") "|"
         (= (get qualified-constructed :tag) "old") "|" (lookupCheck) "|" (callableCheck))))
