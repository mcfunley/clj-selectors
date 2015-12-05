(ns com.mcfunley.selectors
  (:require [clojure.core.match :refer [match]]
            [clojure.core.match.regex :refer :all]))

  

;; =============================================================================
;; parsing

(declare consume-tokens)


(defn tokenize-element
  [token]
  (when token
    (match (re-matches #"(.*)(\[[^\]]+\])(.*)?" token)
      [_ start attr-exp rest] (concat
                                 (tokenize-element start)
                                 (list attr-exp)
                                 (tokenize-element rest))
    
      :else (re-seq #"[.:#]?[^.:#]+" token))))


(defn parse-attribute-selector
  [token]
  :todo-attribute-selector)

(defn parse-pseudo-element
  [token]
  :todo-pseudo-element)

(defn parse-pseudo-class
  [token]
  :todo-pseudo-class)

(defn parse-token
  [token]
  (match token
    "*"           `(match-element-type :any)
    #"^\[.*\]$"   (parse-attribute-selector token)
    #"\.[^:#]+$"  `(match-class ~(subs token 1))
    #"#[^.:]+$"   `(match-id ~(subs token 1))
    #"::.*"       (parse-pseudo-element token)
    #":[^:].*"    (parse-pseudo-class token)
    #"^[^\[:.#]+" `(match-element-type ~token)
    :else         `(match-all ~(map parse-token (tokenize-element token)))))


(defn symbol-to-matcher
  [sym]
  (symbol "com.mcfunley.selectors" (name sym)))


(defn build-binary-op
  [matcher-sym tokens]
  (let [matcher (symbol-to-matcher matcher-sym)
        left-side (parse-token (first tokens))
        right-side (consume-tokens (drop 2 tokens))]
    `(~matcher ~left-side ~right-side)))


(defn consume-tokens
  [tokens]

  (match (vec tokens)
    []          ()
    [a]         (parse-token a)
    [_ ">" _]   (build-binary-op 'match-with-child tokens)
    [_ "+" _]   (build-binary-op 'match-immediately-preceding tokens)
    [_ "~" _]   (build-binary-op 'match-preceding tokens)
    [a & r]     `(match-ancestor ~(parse-token a) ~(consume-tokens r))))


(defn tokenize-selector
  [selector]
  (filter not-empty (clojure.string/split selector #"\s+")))


(defn parse-selector
  [selector]
  (let [tokens (tokenize-selector selector)]
    (consume-tokens tokens)))



;; =============================================================================
;; compiling

(defn compile-selector
  [selector]
  (let [ast (parse-selector selector)]
    ))


;; =============================================================================
;; matching

(defn match-element
  [root parent elem matcher]
  (let [[tag attributes children] elem]
    (filter some? (cons (matcher root parent elem)
                        (map #(match-element root elem %1 matcher) children)))))


(defn $
  [selector tree]
  (let [matcher (compile-selector selector)]
    (match-element tree nil tree matcher)))
