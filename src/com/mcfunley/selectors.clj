(ns com.mcfunley.selectors
  (:require [clojure.core.match :refer [match]]
            [clojure.core.match.regex :refer :all]))


(load "selectors_tokenizing")
(load "selectors_parsing")
(load "selectors_optimize")
(load "selectors_navigation")
(load "selectors_matching")


;; =============================================================================
;; public interface

(defrecord Selector [source expression match]
  clojure.lang.IFn
  (invoke [& args] (match args)))

(defn selector? [x] (isa? (type x) Selector))


(defn- ensure-tree-seq
  [tree]
  (if (and (sequential? tree) (keyword? (first tree)))
    (list tree)
    tree))


(defn compile-selector
  [source]
  (let [expression (parse-selector source)]
    (Selector. source expression (eval expression))))


(defn- ensure-compiled
  [selector]
  (if (selector? selector) selector (compile-selector selector)))


(defn $
  [selector e]

  (let [s (ensure-compiled selector)
        trees (ensure-tree-seq e)
        tree-element-seqs (map element-seq trees)
        match-candidates (map list (flatten-elements tree-element-seqs))
        match-sets (map s match-candidates)]
    
    (flatten-elements match-sets)))
