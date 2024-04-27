(let [pTags (-> (.querySelectorAll document "p") Array.from (.filter (fn [x] (= x.innerText "Tags"))) first)
      parent pTags.parentElement.nextElementSibling.firstElementChild
      children (.from Array parent.children)]
  (.sort children
         (fn [n1 n2]
           (defn- tag_count [n]
             (let [x (second n.children)]
               (-> x.innerText (.replaceAll "," "") parseInt)))
           (- (tag_count n1) (tag_count n2))))
  (while parent.firstChild
    (.removeChild parent parent.firstChild))
  (.forEach
   children
   (fn [child] (.appendChild parent child))))