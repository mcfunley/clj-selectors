(ns com.mcfunley.selectors
  (:require [clojure.core.match :refer [match]]
            [clojure.core.match.regex :refer :all]))


(import java.lang.UnsupportedOperationException)


(defn unsupported
  [msg]
  (throw (UnsupportedOperationException. (str msg))))



;; =============================================================================
;; tokenizing


(defn group-extract
  [pattern s]
  (filter some? (flatten (map rest (re-seq pattern s)))))


(defn extract-quoted
  ;; pulls quoted strings out of the input, e.g.
  ;; (extract-quoted "foo  bar \"baz\"") -> ("foo  bar " "\"baz\"")
  [s]
  (group-extract #"([^\"]+)?(\"[^\"]+\")?" s))


(defn extract-bracketed
  [s]
  (group-extract #"([^\[]+)?(\[[^\[]+\])?" s))



(defn tokenize-selector
  "The first pass tokenizer run on a whole selector expression."
  [selector]

  (defn replace-in-brackets
    [g pattern repl]
    (match g
      #"^\[.*\]$" (clojure.string/replace g pattern repl)
      :else g))

  (defn hack-brackets [g] (replace-in-brackets g #"\s" "\0"))
  (defn unhack-brackets [g] (clojure.string/replace g #"\x00" " "))
  
  (let [bracket-groups (extract-bracketed selector)
        hacked-brackets (map hack-brackets bracket-groups)
        joined (clojure.string/join hacked-brackets)
        element-tokens (clojure.string/split joined #"\s+")]
    (map unhack-brackets (filter not-empty element-tokens))))



(defn tokenize-element
  "Tokenizes an element expression (e.g. `foo.bar#baz`)"
  [el]
  (when el
    (match (re-matches #"(.*)(\[[^\]]+\])(.*)?" el)
      [_ start attr-exp rest] (concat
                                 (tokenize-element start)
                                 (list attr-exp)
                                 (tokenize-element rest))
    
      :else (map first (re-seq #"(::|[.:#])?[^.:#]+" el)))))


(defn tokenize-attribute-selector
  [sel]

  (defn split-words
    [s]
    (match s
      #"^\"[^\"]+\"$" (clojure.string/replace s #"\"" "")
      :else           (clojure.string/split (clojure.string/trim s) #"\s+")))
  
  (let [expr (clojure.string/replace sel #"[\[\]]" "")
        trimmed (clojure.string/trim expr)
        padded (clojure.string/replace trimmed #"([~|$\^*]?=)" " $1 ")
        quoted-separated (extract-quoted padded)]
    (flatten (map split-words quoted-separated))))




;; =============================================================================
;; parsing

(declare consume-tokens)


(defn symbol-to-matcher
  [sym]
  (symbol "com.mcfunley.selectors" (name sym)))


(defn read-attribute-selector-tokens
  [tokens]

  (defn collect-expressions
    [expression rest-tokens]
    (let [rest-expressions (read-attribute-selector-tokens rest-tokens)]
      (cons expression (filter some? (list rest-expressions)))))

  (defn binop
    [matcher-sym a b r]
    (let [matcher (symbol-to-matcher matcher-sym)
          op `(~matcher ~a ~b)]
      (if (empty? r)
        op
        `(match-all ~(collect-expressions op r)))))
       
  (match (vec tokens)
    []             nil
    
    [a "~=" b & r] (binop 'match-attribute-value-contains-word a b r)
    [a "^=" b & r] (binop 'match-attribute-value-begins a b r)
    [a "$=" b & r] (binop 'match-attribute-value-ends a b r)
    [a "*=" b & r] (binop 'match-attribute-value-contains-pattern a b r)
    [a "|=" b & r] (binop 'match-attribute-value-lang-subcode a b r)
    [a "=" b & r]  (binop 'match-attribute-value a b r)
    
    [a & r]        `(match-all
                     ~(collect-expressions `(match-attribute-exists ~a) r))))
                     


(defn parse-attribute-selector
  [sel]
  (read-attribute-selector-tokens (vec (tokenize-attribute-selector sel))))


(defn parse-pseudo-element
  [token]
  (match token
    "::first-line"    `(select-first-line)
    "::first-letter"  `(select-first-letter)
    "::before"        (unsupported "::before pseudo element not supported.")
    "::after"         (unsupported "::after pseudo element not supported.")))


(defn parse-pseudo-class
  [token]
  :todo-pseudo-class)    


(defn parse-token
  [token]
  (match token
    "*"               `(match-element-type :any)
    #"^\[.*\]$"       (parse-attribute-selector token)
    #"^\.[^\[:#]+$"   `(match-class ~(subs token 1))
    #"#[^\[.:]+$"       `(match-id ~(subs token 1))
    #"^::.*"          (parse-pseudo-element token)
    #"^:[^:].*"       (parse-pseudo-class token)
    #"^[^\[:.#]+"     `(match-element-type ~token)
    :else             `(match-all ~(map parse-token (tokenize-element token)))))


(defn consume-tokens
  [tokens]

  (defn binop
    [matcher-sym tokens]
    (let [matcher (symbol-to-matcher matcher-sym)
          left-side (parse-token (first tokens))
          right-side (consume-tokens (drop 2 tokens))]
    `(~matcher ~left-side ~right-side)))

  (match (vec tokens)
    []          ()
    [a]         (parse-token a)
    [_ ">" _]   (binop 'match-with-child tokens)
    [_ "+" _]   (binop 'match-immediately-preceding tokens)
    [_ "~" _]   (binop 'match-preceding tokens)
    [a & r]     `(match-ancestor ~(parse-token a) ~(consume-tokens r))))



;; =============================================================================
;; simplifying

(defn match-all? [x] (= (first x) `match-all))


(defn flatten-match-all
  [match-all-expression]
  (cons `match-all
        (list 
         (filter (complement match-all?)
                 (tree-seq match-all? second match-all-expression)))))
   
(defn simplify
  [expr-tree]

  (defn simplify-expressions
    [argument-list]
    (map (fn [a] (if (sequential? a) (simplify a) a)) argument-list))

  (match (vec expr-tree)
    ;; if we're at a match-all, flatten any children that are match-all into
    ;; the list and recurse down.
    [`match-all _]   (flatten-match-all expr-tree)

    ;; recurse down if its an expression
    [f & arguments]  `(~f ~@(simplify-expressions arguments))

    :else            expr-tree))


;; =============================================================================
;; matching

(defn match-element
  [root parent elem matcher]
  (let [[tag attributes children] elem]
    (filter some? (cons (matcher root parent elem)
                        (map #(match-element root elem %1 matcher) children)))))


;; =============================================================================
;; public interface


(defn compile-selector
  [selector]
  (simplify
   (let [tokens (tokenize-selector selector)]
     (consume-tokens tokens))))


(defn $
  [selector tree]
  (let [matcher (compile-selector selector)]
    (match-element tree nil tree matcher)))
