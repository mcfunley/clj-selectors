(import java.lang.UnsupportedOperationException)

(defn unsupported
  [msg]
  (throw (UnsupportedOperationException. (str msg))))


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
