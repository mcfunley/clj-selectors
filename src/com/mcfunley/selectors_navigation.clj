;; functions for navigating element trees.

(defn tag [elem] (first elem))

(defn attributes [elem] (second elem))

(defn children [elem] (filter sequential? (drop 2 elem)))

(defn element-seq [elem] (tree-seq identity children elem))

(defn descendant-seq [elem] (rest (element-seq elem)))
