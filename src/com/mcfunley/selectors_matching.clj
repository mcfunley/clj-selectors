;; Functions in this file implement element tree matching.
;;
;; The selector compiler yields an expression tree like:
;;
;;    '(match-foo (match-bar "arg") (match-baz 123))
;;
;; This is eval'd to yield a compiled selection function. So generally speaking,
;; every `match-` function takes a set of arguments and returns a function that
;; accepts a list of element trees and returns a list of matching elements.
;;


(defn- drop-non-matching
  [matcher candidates]
  (drop-while #(empty? (matcher %)) candidates))


(defn- keep-matching
  [matcher candidates]
  (filter #(not (empty? (matcher %))) candidates))


(defn- match-in-children
  [set-matcher elem]
  (flatten-elements (set-matcher (match-candidate-seq (children elem)))))



(defn match-element-type
  [name]
  (if (= name :any)
    identity
    (fn [elems] (filter #(= (keyword name) (tag %1)) elems))))


(defn match-id
  [name]
  (fn [elems] (filter #(= (-> %1 attributes :id) name) elems)))


(defn match-class
  [name]
  (def is-name? (partial = name))
  (fn [elems] (filter #(some is-name? (classnames %)) elems)))


(defn match-all
  [& matchers]

  (defn match-all-single
    [elem]
    (let [results (map #(% (list elem)) matchers)]
      (when (every? (complement empty?) results)
        (last results))))
  
  (fn [elems] (flatten-elements (map match-all-single elems))))


(defn match-ancestor
  [ancestor-matcher descendant-matcher]
  
  (defn m
    [elem]
    (and (not (empty? (ancestor-matcher (list elem))))
         (descendant-matcher (descendant-seq elem))))

  (fn [elems] (flatten-elements (map m elems))))


(defn match-with-child
  [parent-matcher child-matcher]

  (defn m
    [elem]
    (and (not (empty? (parent-matcher (list elem))))
         (child-matcher (children elem))))
  
  (fn [elems] (flatten-elements (map m elems))))


(defn- build-set-matcher
  [match-in-set]
  
  (def mc #(match-in-children match-in-set %))

  (fn [elems]
    (let [child-matches (map mc elems)
          passed-matches (match-in-set (match-candidate-seq elems))]
      (flatten-elements (concat child-matches passed-matches)))))


(defn match-preceding
  [left-match right-match]

  (defn match-in-set
    [candidates]
    (let [left (drop-non-matching left-match candidates)]
      (keep-matching right-match (rest left))))

  (build-set-matcher match-in-set))


(defn match-immediately-preceding
  [left-match right-match]

  (defn match-in-set
    [candidates]
    (let [left (drop-non-matching left-match candidates)]
      (keep-matching right-match (list (second left)))))

  (build-set-matcher match-in-set))


(defn- attr-filter
  [f elems]
  (filter #(f (attributes %)) elems))

(defn match-attribute-exists
  [name]
  (fn [elems] (attr-filter (keyword name) elems)))

(defn match-attribute-value
  [name value]
  (fn [elems] (attr-filter #(= ((keyword name) %) value) elems)))

(defn match-attribute-value-contains-word
  [name word]
  (def is-word? (partial = word))
  (fn [elems] (filter #(some is-word? (attribute-words % name)) elems)))


(defn match-attribute-value-begins
  [name beginning]
  
  (letfn [(begins? [^String attrval]
          (when attrval
            (.startsWith attrval beginning)))]
    
    (fn [elems] (attr-filter #(begins? ((keyword name) %)) elems))))

(defn match-attribute-value-ends
  [name ending]

  (letfn [(ends? [^String attrval]
            (when attrval
              (.endsWith attrval ending)))]

    (fn [elems] (attr-filter #(ends? ((keyword name) %)) elems))))

(defn match-attribute-value-contains-pattern
  [name substr]

  (letfn [(contains? [^String attrval]
            (when attrval
              (.contains attrval substr)))]

    (fn [elems] (attr-filter #(contains? ((keyword name) %)) elems))))

(defn match-attribute-value-lang-subcode
  [name code]

  (letfn [(match? [^String attrval]
            (when attrval
              (or (.startsWith attrval code)
                  (.startsWith attrval (clojure.string/join '(code "-"))))))]
  
    (fn [elems] (attr-filter #(match? ((keyword name) %)) elems))))


(defn select-first-letter
  []
  (fn [elems] (map #(first (first (text-node-seq %))) elems)))


(defn match-not
  [selector-fun]
  (fn [elems] (filter #(empty? (selector-fun (list %))) elems)))
