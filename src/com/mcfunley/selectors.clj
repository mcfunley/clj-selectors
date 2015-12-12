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

(defn compile-selector
  [source]
  (let [expression (parse-selector source)]
    (Selector. source expression (eval expression))))

(defn $
  [selector tree]
  (let [s (if (selector? selector) selector (compile-selector selector))]
    (coalesce-matches (map s (element-seq tree)))))


