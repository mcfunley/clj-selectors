;; Functions for breaking up strings of text in various parts of selector
;; expressions into sequences of digestible tokens.
;; 


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

  (letfn [(replace-in-brackets
            [g pattern repl]
            (match g
              #"^\[.*\]$" (clojure.string/replace g pattern repl)
              :else g))

          (hack-brackets [g] (replace-in-brackets g #"\s" "\0"))

          (unhack-brackets [g] (clojure.string/replace g #"\x00" " "))]
  
    (let [bracket-groups (extract-bracketed selector)
          hacked-brackets (map hack-brackets bracket-groups)
          joined (clojure.string/join hacked-brackets)
          element-tokens (clojure.string/split joined #"\s+")]
      (map unhack-brackets (filter not-empty element-tokens)))))



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
  "Tokenizes an attribute selector (e.g. the expression found within brackets in 
  `foo.bar[baz=\"goo\"]`)."
  [sel]

  (letfn [(split-ws [s] (clojure.string/split (clojure.string/trim s) #"\s+"))
          
          (split-words
            [s]
            (match s
              #"^\"[^\"]+\"$" (clojure.string/replace s #"\"" "")
              :else           (split-ws s)))]
  
    (let [expr (clojure.string/replace sel #"[\[\]]" "")
          trimmed (clojure.string/trim expr)
          padded (clojure.string/replace trimmed #"([~|$\^*]?=)" " $1 ")
          quoted-separated (extract-quoted padded)]
      (flatten (map split-words quoted-separated)))))


(defn tokenize-pseudo-class
  "Tokenizes a pseudo-class string, e.g. :foo(bar). Returns a vector of 
  length 2. The first element is the pseudo-class operator, and 
  the second element is the parenthesized expression, if any."
  ;; These are simple single colon-prefixed words, possibly with a suffix
  ;; in parens.
  [token]

  (if-let [paren-match (re-find #"^(:[a-zA-Z\-]+)\((.*)\)$" token)]
    (vec (drop 1 paren-match))

    (if-let [simple-match (re-find #"^(:[a-zA-Z\-]+)$" token)]
      (conj (vec (drop 1 simple-match)) nil)

      (parse-error (format "Unrecognized pseudo-class: %s" token)))))

