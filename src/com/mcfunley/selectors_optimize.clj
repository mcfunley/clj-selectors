;; Expression optimization functions.
;;
;; Right now the only optimization performed is to simplify match-all
;; expressions, which is done mainly to make the expression trees more
;; readable. So we turn this:
;;
;;   `(match-all m1 m2 (match-all m3 (match-all m4 m5) m6))
;;
;; into this:
;;
;;   `(match-all m1 m2 m3 m4 m5 m6)
;;


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
