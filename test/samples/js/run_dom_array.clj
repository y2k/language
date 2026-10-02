;; true|true|true|true
(raw-code "const hostTrace = []; const buttons = []; const originalThreads = [0, 1, 2].map(id => {
  const button = { click() {
    if (this !== buttons[id]) throw new Error('button identity');
    hostTrace.push('click' + id);
  }};
  buttons.push(button);
  const thread = { querySelector(selector) {
    if (this !== originalThreads[id] || selector !== '.post__btn_type_hide') throw new Error('thread or selector');
    hostTrace.push('query' + id);
    return button;
  }};
  return thread;
});
const threads = Array.from(originalThreads);")

(defn- hide-thread! [thread]
  (.click (.querySelector thread ".post__btn_type_hide")))

(defn test []
  (let [result (run! hide-thread! threads)]
    (str (= (str hostTrace) "(query0 click0 query1 click1 query2 click2)") "|"
         (= result nil) "|" (not= result "nil") "|" (not result))))
