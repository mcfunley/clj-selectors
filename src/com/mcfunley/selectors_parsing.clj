;; The functions in this file accept sequences of tokens and yield match
;; expression trees.
;;


(declare
 consume-tokens
 simplify
 parse-selector)


(defn symbol-to-matcher
  [sym]
  (symbol "com.mcfunley.selectors" (name sym)))


(defn read-attribute-selector-tokens
  [tokens]

  (letfn [(collect-expressions
            [expression rest-tokens]
            (let [rest-expressions (read-attribute-selector-tokens rest-tokens)]
              (cons expression (filter some? (list rest-expressions)))))

          (binop
            [matcher-sym a b r]
            (let [matcher (symbol-to-matcher matcher-sym)
                  op `(~matcher ~a ~b)]
              (if (empty? r)
                op
                `(match-all ~@(collect-expressions op r)))))]
       
    (match (vec tokens)
      []             nil
    
      [a "~=" b & r] (binop 'match-attribute-value-contains-word a b r)
      [a "^=" b & r] (binop 'match-attribute-value-begins a b r)
      [a "$=" b & r] (binop 'match-attribute-value-ends a b r)
      [a "*=" b & r] (binop 'match-attribute-value-contains-pattern a b r)
      [a "|=" b & r] (binop 'match-attribute-value-lang-subcode a b r)
      [a "=" b & r]  (binop 'match-attribute-value a b r)
    
      [a & r]        `(match-all
                       ~@(collect-expressions `(match-attribute-exists ~a) r)))))
                     


(defn parse-attribute-selector
  [sel]
  (read-attribute-selector-tokens (vec (tokenize-attribute-selector sel))))


(defn parse-pseudo-element
  [token]
  (match token
    "::first-line"    (unsupported "::first-line pseudo-element not supported.")
    "::first-letter"  `(select-first-letter)
    "::before"        (unsupported "::before pseudo element not supported.")
    "::after"         (unsupported "::after pseudo element not supported.")))


(defn- throw-unsupported-simple-pseudo-class
  [cls]
  (when 
      (#{ ":link" ":visited" ":active" ":hover" ":focus" ":target" ":enabled" 
         ":disabled" ":checked" ":root" } cls)
    (unsupported (format "%s pseudo-class not supported." cls))))


(defn parse-pseudo-class
  [token]
  (throw-unsupported-simple-pseudo-class token)

  (let [[operator arg] (tokenize-pseudo-class token)]
    (case operator
      ":lang"             (unsupported ":lang pseudo-class not supported")
      ":nth-child"        :todo
      ":nth-last-child"   :todo
      ":nth-of-type"      :todo
      ":nth-last-of-type" :todo
      ":first-child"      :todo
      ":last-child"       :todo
      ":only-child"       :todo
      ":only-of-type"     :todo
      ":empty"            :todo
      ":not"              `(match-not ~(parse-selector arg))
      
      (parse-error (format "Unrecognized pseudo-class expression %s" token)))))


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

  (letfn [(binop
            [matcher-sym tokens]
            (let [matcher (symbol-to-matcher matcher-sym)
                  left-side (parse-token (first tokens))
                  right-side (consume-tokens (drop 2 tokens))]
              `(~matcher ~left-side ~right-side)))]

    (match (vec tokens)
      []            ()
      [a]           (parse-token a)
      [_ ">" & r]   (binop 'match-with-child tokens)
      [_ "+" & r]   (binop 'match-immediately-preceding tokens)
      [_ "~" & r]   (binop 'match-preceding tokens)
      [a & r]       `(match-ancestor ~(parse-token a) ~(consume-tokens r)))))


(defn parse-selector
  [selector]
  (simplify
   (let [tokens (tokenize-selector selector)]
     (consume-tokens tokens))))
