;; Functions for navigating element trees. Note that tagsoup provides
;; some of these, but declaring them ourselves lets us avoid taking the
;; dependency.
;;
;; Element trees from tagsoup look like this:
;;
;;    [:foo {:attr1 "x"} [:child {}] [:child {:attr2 "y"}] "text"]
;;

(defn tag [elem] (first elem))

(defn attributes [elem] (second elem))

(defn children [elem] (filter sequential? (drop 2 elem)))

(defn element-seq [elem] (tree-seq identity children elem))

(defn descendant-seq [elem] (rest (element-seq elem)))
