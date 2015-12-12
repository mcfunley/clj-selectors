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

(declare consume-tokens simplify)


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
        `(match-all ~@(collect-expressions op r)))))
       
  (match (vec tokens)
    []             nil
    
    [a "~=" b & r] (binop 'match-attribute-value-contains-word a b r)
    [a "^=" b & r] (binop 'match-attribute-value-begins a b r)
    [a "$=" b & r] (binop 'match-attribute-value-ends a b r)
    [a "*=" b & r] (binop 'match-attribute-value-contains-pattern a b r)
    [a "|=" b & r] (binop 'match-attribute-value-lang-subcode a b r)
    [a "=" b & r]  (binop 'match-attribute-value a b r)
    
    [a & r]        `(match-all
                     ~@(collect-expressions `(match-attribute-exists ~a) r))))
                     


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
    :else             `(match-all ~@(map parse-token (tokenize-element token)))))


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


(defn parse-selector
  [selector]
  (simplify
   (let [tokens (tokenize-selector selector)]
     (consume-tokens tokens))))


;; =============================================================================
;; simplifying

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



;; =============================================================================
;; MatchContext

(defrecord MatchContext [tag attributes children root parent])

(defn make-context
  ([[tag attributes & children]]
   (MatchContext. tag attributes children nil nil))

  ([[tag attributes & children] root parent]
   (MatchContext. tag attributes children root parent)))

(defn element [context]
  (vec (concat ((juxt :tag :attributes) context) (:children context))))

(defn root [context] (or (:root context) context))

(defn children
  [context]
  (map #(make-context %1 (root context) context) (:children context)))

(defn context-seq [context] (tree-seq identity children context))

(defn descendant-seq [context] (rest (context-seq context)))


;; =============================================================================
;; matching

(defn- evaluate-matcher-predicate
  [predicate]
  (fn [context]
    (if (predicate context)
      (list (element context))
      nil)))

(defmacro defmatcher
  [name arglist predicate]
  `(defn ~name ~arglist (evaluate-matcher-predicate ~predicate)))
    

(defmatcher match-element-type
  [name]
  (let [tag-key (keyword name)]
    (fn [context] (= (:tag context) tag-key))))

(defmatcher match-id
  [name]
  (fn [context] (= (-> context :attributes :id) name)))

(defmatcher match-class
  [name]
  (fn [context]
    (let [class-attr (-> context :attributes :class)]
      (when class-attr
        (some #(= name %) (clojure.string/split class-attr #"\s+"))))))

(defn match-all
  [& matchers]
  (fn [context]
    (let [results (map #(%1 context) matchers)]
      (when (every? identity results)
        (last results)))))


(defn match-descendants
  [descendant-matcher context]
  (let [matches (map #(descendant-matcher %1) (descendant-seq context))]
    (map first (filter some? matches))))

(defn match-ancestor
  [ancestor-matcher descendant-matcher]
  (fn [context]
    (and (ancestor-matcher context)
         (match-descendants descendant-matcher context))))

(defn match-with-child
  [ancestor-matcher child-matcher]
  (fn [context]
    ; todo
    nil))


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
  (let [s (if (selector? selector) selector (compile-selector selector))
        root-context (make-context tree)
        raw (map s (context-seq root-context))]
    (reduce concat (filter identity raw))))



