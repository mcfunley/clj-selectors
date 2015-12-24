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


(defn match-attribute-exists
  [name]
  (fn [elems] (filter #((keyword name) (attributes %)) elems)))
