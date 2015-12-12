;; Functions in this file implement element tree matching.
;;
;; The selector compiler yields an expression tree like:
;;
;;    '(match-foo (match-bar "arg") (match-baz 123))
;;
;; This is eval'd to yield a compiled selection function. So generally speaking,
;; every `match-` function takes a set of arguments and returns a function that
;; accepts an element tree and returns a list of matching elements.
;; 
  

(defn- evaluate-matcher-predicate
  [predicate]
  (fn [elem]
    (if (predicate elem)
      (list elem)
      nil)))

(defmacro defmatcher
  [name arglist predicate]
  `(defn ~name ~arglist (evaluate-matcher-predicate ~predicate)))

(defn coalesce-matches
  [match-result-list]
  (reduce concat (filter identity match-result-list)))


(defmatcher match-element-type
  [name]
  (let [tag-key (keyword name)]
    (fn [elem] (= (tag elem) tag-key))))

(defmatcher match-id
  [name]
  (fn [elem] (= (-> elem attributes :id) name)))

(defmatcher match-class
  [name]
  (fn [elem]
    (let [class-attr (-> elem attributes :class)]
      (when class-attr
        (some #(= name %) (clojure.string/split class-attr #"\s+"))))))

(defn match-all
  [& matchers]
  (fn [elem]
    (let [results (map #(%1 elem) matchers)]
      (when (every? identity results)
        (last results)))))


(defn match-descendants
  [descendant-matcher elem]
  (let [matches (map #(descendant-matcher %1) (descendant-seq elem))]
    (map first (filter some? matches))))

(defn match-ancestor
  [ancestor-matcher descendant-matcher]
  (fn [elem]
    (and (ancestor-matcher elem)
         (match-descendants descendant-matcher elem))))

(defn match-with-child
  [parent-matcher child-matcher]
  (fn [elem]
    (and (parent-matcher elem)
         (coalesce-matches (map child-matcher (children elem))))))

;; (defn match-preceding
;;   [left-match right-match]
;;   (fn [context]
