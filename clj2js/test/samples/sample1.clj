(let [pTags (-> (.querySelectorAll document "p") Array/from (.filter (fn [x] (= x.innerText "Tags"))) first)
      parent (-> pTags .parentElement .nextElementSibling .firstElementChild)
      children (Array/from (.children parent))]
  (.sort children
         (fn [n1 n2]
           (defn tag_count [n]
             (-> n.children second .innerText (.replaceAll "," "") parseInt))
           (- (tag_count n1) (tag_count n2))))
  (while parent.firstChild
    (.removeChild parent (.firstChild parent)))
  (.forEach
   children
   (fn [child] (.appendChild parent child))))