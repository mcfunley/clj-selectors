(defn match-all? [x] (and (sequential? x) (= (first x) `match-all)))


(defn flatten-match-all
  [expr]
  (if (match-all? expr)
    (cons `match-all
          (filter (complement match-all?) (tree-seq match-all? rest expr)))
    
    expr))


(defn simplify
  [expr]
  (if (sequential? expr)
    (let [flattened-expr (flatten-match-all expr)
          f (first flattened-expr)
          args (rest flattened-expr)]
      `(~f ~@(map simplify args)))
    expr))
