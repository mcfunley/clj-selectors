(ns com.mcfunley.selectors-test
  (:require [clojure.test :refer :all]
            [com.mcfunley.selectors :refer :all]
            [pl.danieljanus.tagsoup]))

(alias 'selectors 'com.mcfunley.selectors)
(alias 'ts 'pl.danieljanus.tagsoup)
(import 'com.mcfunley.selectors.Selector)


;; ==============================================================================
;; compilation tests

(deftest parse-any
  (is (= `(selectors/match-element-type :any)
         (parse-selector "*"))))

(deftest parse-element-type
  (is (= `(selectors/match-element-type "foo")
         (parse-selector "foo"))))

(deftest parse-with-child
  (is (= `(selectors/match-with-child
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (parse-selector "foo > bar"))))

(deftest parse-immediately-preceding
  (is (= `(selectors/match-immediately-preceding
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (parse-selector "foo + bar"))))

(deftest parse-preceding
  (is (= `(selectors/match-preceding
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (parse-selector "foo ~ bar"))))

(deftest parse-extra-whitespace
  (is (= `(selectors/match-preceding
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (parse-selector "foo   ~ bar "))))

(deftest parse-ancestor
  (is (= `(selectors/match-ancestor
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (parse-selector "foo bar"))))

(deftest parse-ancestor-class-match
  (is (= `(selectors/match-ancestor
           (selectors/match-element-type "foo")
           (selectors/match-class "bar"))
         (parse-selector "foo .bar"))))

(deftest parse-type-and-id
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-id "bar"))
         (parse-selector "foo#bar"))))

(deftest parse-type-id-and-class
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-id "bar")
           (selectors/match-class "baz"))
         (parse-selector "foo#bar.baz"))))

(deftest parse-any-with-class
  (is (= `(selectors/match-all
           (selectors/match-element-type :any)
           (selectors/match-class "foo"))
         (parse-selector "*.foo"))))

(deftest parse-first-line
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/select-first-line)))
      (parse-selector "foo::first-line")))

(deftest parse-first-letter
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-class "bar")
           (selectors/select-first-letter)))
      (parse-selector "foo.bar::first-letter")))


(deftest parse-having-attribute
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-attribute-exists "bar"))
         (parse-selector "foo[bar]"))))

(deftest parse-having-two-attributes
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-attribute-exists "bar")
           (selectors/match-attribute-exists "baz"))
         (parse-selector "foo[bar baz]"))))

(deftest parse-attribute-contains-word
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-attribute-value-contains-word "bar" "baz"))
         (parse-selector "foo[bar ~= \"baz\"]"))))

(deftest parse-attribute-value-begins
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-class "bar")
           (selectors/match-attribute-value-begins "baz" "fizz buzz"))
         (parse-selector "foo.bar[baz^=\"fizz buzz\"]"))))

(deftest parse-attribute-value-ends
  (is (= `(selectors/match-all
           (selectors/match-element-type :any)
           (selectors/match-attribute-value-ends "foo" "bar"))
         (parse-selector "*[foo $= \"bar\"]"))))

(deftest parse-attribute-value-contains-pattern
  (is (= `(selectors/match-all
           (selectors/match-element-type :any)
           (selectors/match-id "crap")
           (selectors/match-attribute-value-contains-pattern "foo" "bar"))
         (parse-selector "*#crap[foo *=   \"bar\"]"))))

(deftest parse-wacky-attribute-value
  (is (= `(selectors/match-all
           (selectors/match-class "foo")
           (selectors/match-attribute-exists "bar")
           (selectors/match-attribute-value-lang-subcode "baz" "en")
           (selectors/match-attribute-exists "goo"))
         (parse-selector ".foo[bar baz|=\"en\" goo  ]"))))

(deftest parse-match-attribute-value
  (is (= `(selectors/match-all
           (selectors/match-id "foo")
           (selectors/match-attribute-exists "bar")
           (selectors/match-attribute-value "baz" "goo ball")
           (selectors/match-attribute-value-contains-pattern "qux" "quick"))
         (parse-selector "#foo[bar baz=\"goo ball\" qux *= \"quick\"]"))))

(deftest parse-complicated-first-line
  (is (= `(selectors/match-ancestor
           (selectors/match-all
            (selectors/match-id "foo")
            (selectors/match-attribute-value "bar" "baz"))
           (selectors/match-all
            (selectors/match-class "qux")
            (selectors/select-first-line)))
         (parse-selector "#foo[bar=\"baz\"] .qux::first-line"))))

(deftest parse-first-line
  (is (= `(selectors/match-all
           (selectors/match-class "foo")
           (selectors/select-first-line))
         (parse-selector ".foo::first-line"))))

(deftest parse-first-letter
  (is (= `(selectors/match-with-child
           (selectors/match-element-type "foo")
           (selectors/match-all
            (selectors/match-id "bar")
            (selectors/select-first-letter)))
         (parse-selector "foo > #bar::first-letter"))))

;; ==============================================================================
;; tokenizing tests

; tokenize-selector

(deftest tokenize-selector-simple
  (is (= (tokenize-selector "foo") '("foo"))))

(deftest tokenize-selector-classname
  (is (= (tokenize-selector ".foo") '(".foo"))))

(deftest tokenize-selector-whitespace
  (is (= (tokenize-selector "  foo .bar") '("foo" ".bar"))))

(deftest tokenize-selector-with-attribute-selector
  (is (= (tokenize-selector "foo[bar] #baz") '("foo[bar]" "#baz"))))

(deftest tokenize-selector-attribute-selector-whitespace
  (is (= (tokenize-selector "foo[bar baz].goo") '("foo[bar baz].goo"))))

(deftest tokenize-selector-attribute-selector-whitespace-multiple
  (is (= (tokenize-selector "foo[bar baz] .goo") '("foo[bar baz]" ".goo"))))

(deftest tokenize-selector-wacky-attribute-selector
  (is (= (tokenize-selector ".foo[bar baz|=\"en\" goo  ]")
         '(".foo[bar baz|=\"en\" goo  ]"))))

(deftest tokenize-child-and-predecessor-selector
  (is (= (tokenize-selector "b.foo > c ~ .foo")
         '("b.foo" ">" "c" "~" ".foo"))))


; tokenize-element

(deftest tokenize-element-attribute-selector
  (is (= (tokenize-element "foo[bar]")
         '("foo" "[bar]"))))

(deftest tokenize-attribute-selector-with-whitespace
  (is (= (tokenize-element "foo[bar baz].goo")
         '("foo" "[bar baz]" ".goo"))))

(deftest tokenize-attribute-selector-with-whitespace-2
  (is (= (tokenize-element "foo[ bar baz *=goo  fizz].goo")
         '("foo" "[ bar baz *=goo  fizz]" ".goo"))))

(deftest tokenize-id-and-class
  (is (= (tokenize-element "foo.bar#baz")
         '("foo" ".bar" "#baz"))))

(deftest tokenize-class-and-id
  (is (= (tokenize-element "foo#bar.baz")
         '("foo" "#bar" ".baz"))))

(deftest tokenize-class-id-filter-exp
  (is (= (tokenize-element "foo[bar].baz#goo")
         '("foo" "[bar]" ".baz" "#goo"))))

(deftest tokenize-pseudo-element
  (is (= (tokenize-element "foo::bar")
         '("foo" "::bar"))))

(deftest tokenize-element-wacky-attribute-selector
  (is (= (tokenize-element ".foo[bar baz|=\"en\" goo  ]")
         '(".foo" "[bar baz|=\"en\" goo  ]"))))


; tokenize-attribute-selector

(deftest tokenize-attribute-selector-1
  (is (= (tokenize-attribute-selector "[foo bar~=\"baz\"]")
         '("foo" "bar" "~=" "baz"))))

(deftest tokenize-attribute-selector-attrname
  (is (= (tokenize-attribute-selector "[foo]")
         '("foo"))))

(deftest tokenize-attribute-selector-internal-spaces
  (is (= (tokenize-attribute-selector "[foo bar ^= \"baz\"   goo]")
         '("foo" "bar" "^=" "baz" "goo"))))

(deftest tokenize-attribute-selector-trimmed
  (is (= (tokenize-attribute-selector "[  foo bar ]")
         '("foo" "bar"))))

(deftest tokenize-attribute-selector-quoted-string
  (is (= (tokenize-attribute-selector "[foo  bar=\"baz  goo\"]")
         '("foo" "bar" "=" "baz  goo"))))

(deftest tokenize-attribute-selector-ending-spaces-ends-with-quote
  (is (= (tokenize-attribute-selector "[foo*=\"baz\" goo bar=\" adf \"  ]")
         '("foo" "*=" "baz" "goo" "bar" "=" " adf "))))
          
(deftest tokenize-attribute-selector-language-subcode
  (is (= (tokenize-attribute-selector "[foo|=\"bar\"]")
         '("foo" "|=" "bar"))))

(deftest tokenize-wacky-attribute-selector
  (is (= (tokenize-attribute-selector "[bar baz|=\"en\" goo  ]")
         '("bar" "baz" "|=" "en" "goo"))))


;; ==============================================================================
;; random regressions

(deftest parse-attribute-selector-match-all-nesting
  (is (= `(selectors/match-all
           (selectors/match-attribute-exists "foo")
           (selectors/match-all
            (selectors/match-attribute-exists "bar")))
         (parse-attribute-selector "[foo bar]"))))

(deftest parse-wacky-attribute-selector
  (is (= `(selectors/match-all
           (selectors/match-attribute-exists "bar")
           (selectors/match-all
            (selectors/match-attribute-value-lang-subcode "baz" "en")
            (selectors/match-all
             (selectors/match-attribute-exists "goo"))))
         (parse-attribute-selector "[bar baz|=\"en\" goo  ]"))))


;; ==============================================================================
;; simplification

(deftest flatten-match-all-nop
  (is (= '(com.mcfunley.selectors/match-all (foo bar))
         (flatten-match-all
          '(com.mcfunley.selectors/match-all (foo bar)))
          )))

(deftest flatten-match-all-one-level
  (is (= '(com.mcfunley.selectors/match-all
           (foo bar)
           (baz goo)
           (fizz buzz)
           (qux quick))

         (flatten-match-all
          '(com.mcfunley.selectors/match-all
            (foo bar)
            (com.mcfunley.selectors/match-all
             (baz goo)
             (fizz buzz)
             (qux quick)))))))

(deftest flatten-match-all-two-levels
  (is (= '(com.mcfunley.selectors/match-all
           (foo bar)
           (baz goo)
           (fizz buzz)
           (qux quick)
           (quack quack))

         (flatten-match-all
          '(com.mcfunley.selectors/match-all
            (foo bar)
             
            (com.mcfunley.selectors/match-all
             (com.mcfunley.selectors/match-all (baz goo))
             (com.mcfunley.selectors/match-all (fizz buzz))
             (qux quick))

            (com.mcfunley.selectors/match-all (quack quack)))))))


(deftest simplify-simple
  (is (= '(foo (bar baz))
         (simplify '(foo (bar baz))))))

(deftest simplify-match-all
  (is (= '(com.mcfunley.selectors/match-all
           (foo bar)
           (baz goo))

         (simplify
          '(com.mcfunley.selectors/match-all
            (foo bar)
            (com.mcfunley.selectors/match-all (baz goo)))))))

(deftest simplify-buried-match-alls
  (is (= '(foo
           (bar (com.mcfunley.selectors/match-all (baz goo) (fizz buzz)))
           (bar (quack quack)))

         (simplify
          '(foo
            (bar (com.mcfunley.selectors/match-all
                  (com.mcfunley.selectors/match-all (baz goo))
                  (fizz buzz)))
            (bar (quack quack)))))))

(deftest simplify-single-select
  (is (= '(com.mcfunley.selectors/select-first-line)
         (simplify '(com.mcfunley.selectors/select-first-line)))))


;; ==============================================================================
;; compiling

(deftest compile-sets-source
  (is (= "foo > bar"
         (:source (compile-selector "foo > bar")))))

(deftest compile-sets-expression
  (is (= `(selectors/match-with-child
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (:expression (compile-selector "foo > bar")))))

(deftest compile-type-and-class
  (is (= `(selectors/match-all
           (selectors/match-element-type "foo")
           (selectors/match-class "bar"))
         (:expression (compile-selector "foo.bar")))))

(deftest compile-child-and-predecessor
  (is (= `(selectors/match-with-child
           (selectors/match-all
            (selectors/match-element-type "b")
            (selectors/match-class "foo"))
           (selectors/match-preceding
            (selectors/match-element-type "c")
            (selectors/match-class "foo")))
         (:expression (compile-selector "b.foo > c ~ .foo")))))


;; ==============================================================================
;; selector?

(deftest selector?-identifies-selector
  (is (selector? (Selector. "foo" '(expr and stuff) identity))))

(deftest selector?-works-nil (is (not (selector? nil))))

(deftest selector?-works-number (is (not (selector? 1))))


;; ==============================================================================
;; navigation

(deftest children-works
  (let [cs [[:bar {}] [:baz {}]]
        el (vec `(:foo {} ~@cs))]
    (is (= (children el) cs))))

(deftest children-element-has-text-nodes
  (is (= '(:a :b)
         (map tag (children [:x {} [:a {}] "foo" [:b {}] "bar"])))))

(deftest no-children
  (is (empty? (children [:foo {}]))))

(deftest descendant-seq-works
  (let [tree [:foo {} [:bar {}] [:baz {} [:goo {}]]]]
    (is (= '(:bar :baz :goo)
           (map tag (descendant-seq tree))))))

(deftest descendant-seq-no-children
  (is (empty? (descendant-seq [:foo {}]))))

(deftest element-seq-works
  (let [tree [:a {} [:b {}] [:c {} [:d {}]]]]
    (is (= '(:a :b :c :d) (map tag (element-seq tree))))))

(deftest element-seq-multiple-branches
  (let [tree [:a {}
              [:b {}]
              [:c {} [:d {}]]
              [:e {} [:f {}
                      [:g {}]]]]]
    (is (= '(:a :b :c :d :e :f :g)
           (map tag (element-seq tree))))))



;; ==============================================================================
;; Selector

(deftest selector-invocation
  (is (= '(:foo) ((Selector. "" nil #(list %1)) :foo))))


;; ==============================================================================
;; matchers

(deftest match-element-type-works
  (is ((match-element-type "foo") [:foo {}])))

(deftest match-element-type-miss
  (is (empty? ((match-element-type "foo") (list [:bar {}])))))

(deftest match-element-type-returns-element
  (let [element [:foo {}]]
    (is (= ((match-element-type "foo") (list element))
           (list element)))))

(deftest match-id-works
  (is ((match-id "bar") (list [:foo { :id "bar" }]))))

(deftest match-id-miss
  (is (empty? ((match-id "baz") (list [:foo { :id "bar" }])))))

(deftest match-id-returns-element
  (let [element [:foo { :id "bar" }]]
    (is (= ((match-id "bar") (list element)) (list element)))))

(deftest match-id-no-id-attribute
  (is (empty? ((match-id "baz") (list [:foo {}])))))

(deftest match-all-works
  (is (let [expr (match-all
                  (match-element-type "foo")
                  (match-id "bar"))]
        (expr (list [:foo { :id "bar" }])))))

(deftest match-all-fail
  (is (empty? (let [expr (match-all
                          (match-element-type "foo")
                          (match-id "bar"))]
                (expr (list [:foo { :id "baz" }]))))))


(deftest match-all-with-element-seq
  (let [elems '([:b {:class "x"} [:c {:class "x y"}]]
                [:c {:class "x y"}]
                [:d {} [:e {:class "y x z"}] [:b {:class "a b x"}]]
                [:e {:class "y x z"}]
                [:b {:class "a b x"}])
        m (match-all (match-element-type "b") (match-class "x"))]
    
    (is (= '([:b {:class "x"} [:c {:class "x y"}]]
             [:b {:class "a b x"}])
           (m elems)))))
           

(deftest match-class-works-one-class
  (is ((match-class "foo") (list [:div { :class "foo" }]))))

(deftest match-class-miss
  (is (empty? ((match-class "foo") (list [:div { :class "bar" }])))))

(deftest match-class-many-classes
  (is ((match-class "foo") (list [:div { :class "bar foo" }]))))

(deftest match-class-whitespace-significant
  (is (empty? ((match-class "foo") (list [:div { :class "bar foobar" }])))))

(deftest match-class-no-class-attr
  (is (empty? ((match-class "foo") (list [:div {}])))))

(deftest match-class-case-sensitive
  (is (empty? ((match-class "foo") (list [:div { :class "Foo" }])))))

(deftest match-class-returns-element
  (let [element [:div { :class "foo bar baz" }]]
    (is (= ((match-class "bar") (list element)) (list element)))))

(deftest match-ancestor-works
  (let [expr (match-ancestor (match-element-type "foo") (match-id "goo"))
        tree [:foo {} [:bar {} [:baz { :id "goo" }]]]]
    (is (= (list [:baz { :id "goo" }])
           (expr (list tree))))))

(deftest match-ancestor-match-class
  (let [expr (match-ancestor (match-element-type "foo") (match-class "goo"))
        tree [:foo {} [:bar { :class "goo" }]]]

    (is (= '([:bar { :class "goo" }]) (expr (list tree))))))

(deftest match-ancestor-multiple-matches
  (let [expr (match-ancestor (match-element-type "foo") (match-class "goo"))
        tree [:foo {}
              [:bar { :class "goo" }
               [:baz { :class "goo ball" }]
               [:qux {}]
               [:fizz { :class "boy-named-goo" }]]]
        result (expr (list tree))
        result-tags (map first result)]

    (is (= '(:bar :baz) result-tags))))

(deftest match-ancestor-multiple-branches
  (let [tree [:a {}
              [:b {:class "x"}
               [:c {:class "x y"}]]
              [:d {}
               [:e {:class "y x z"}]]]
        expr (match-ancestor (match-element-type "a") (match-class "x"))
        result (expr (list tree))]
    (is (= '(:b :c :e) (map first result)))))


;; ==============================================================================
;; invoking selectors

(deftest invoke-match-all-type-and-class-miss
  (let [s (compile-selector "a.b")]
    (is (empty? (s (list [:a {} [:b {:class "x"}]]))))))


;; ==============================================================================
;; flatten-elements

(deftest flatten-elements-works
  (is (= '([:a {}] [:b {}] [:c {}])
         (flatten-elements '(nil ([:a {}]) ([:b {}] [:c {}]) nil nil)))))
  

;; ==============================================================================
;; $

(deftest $-works
  (let [tree [:a {} [:b { :id "foo" }]]]
    (is (= '([:b { :id "foo" }])
           ($ "#foo" tree)))))

(deftest $-multiple-results
  (let [tree [:a {}
              [:b {:class "x"}
               [:c {:class "x y"}]]
              [:d {}
               [:e {:class "y x z"}]]]

        result ($ ".x" tree)]
    (is (= '(:b :c :e) (map first result)))))

(deftest $-ancestor-multiple-results
  (let [tree [:a {}
              [:b {:class "x"}
               [:c {:class "x y"}]]
              [:d {}
               [:e {:class "y x z"}]
               [:b {:class "a b x"}]]]
        result ($ "a b.x" tree)]
    (is (= '([:b {:class "x"} [:c {:class "x y"}]]
             [:b {:class "a b x"}])
           result))))

(deftest $-child-match
  (let [tree [:a {} [:b {:class "foo"}] [:c {}]]]
    (is (= '([:b {:class "foo"}])
           ($ "a > .foo" tree)))))

(deftest $-child-no-match
  (let [tree [:a {} [:b {:class "foo"}] [:c {}]]]
    (is (empty? ($ "a > .bleh" tree)))))

(deftest $-predecessor
  (let [tree [:a {} [:b {:class "foo"}] [:c {}] [:d {:class "foo"}]]]
    (is (= '(:d) (map tag ($ "c ~ .foo" tree))))))

(deftest $-predecessor-extra-in-rest
  (let [tree [:a {} [:b {:class "foo"}] [:c {}] [:d {:class "foo"}]]]
    (is (= '(:c) (map tag ($ "b ~ c" tree))))))

(deftest $-match-element-type-*
  (let [tree [:a {} [:b {:class "foo"}] [:c {}] [:d {:class "foo"}]]]
    (is (= '(:a :b :c :d) (map tag ($ "*" tree))))))

(deftest $-predecessor-miss
  (let [tree [:a {} [:b {:class "foo"}] [:c {}] [:d {:class "foo"}]]]
    (is (empty? ($ "c ~ b.foo" tree)))))

(deftest $-immediately-preceding
  (let [tree [:a {} [:b {:class "foo"}] [:c {}] [:d {:class "foo"}]]]
    (is (= '(:c) (map tag ($ "b.foo + *" tree))))))

(deftest $-immediately-preceding-extra-in-rest
  (let [tree [:a {} [:b {:class "foo"}] [:c {}] [:d {:class "foo"}]]]
    (is (= '(:c) (map tag ($ "b + c" tree))))))

(deftest $-match-child-and-predecessor
  (let [tree [:a {}
              [:b {:class "foo"}
               [:c {}] [:d {:class "foo"}] [:e {} ] [:f {:class "foo"}]]]]
    (is (= '([:d {:class "foo"}] [:f {:class "foo"}])
           ($ "b.foo > c ~ .foo" tree)))))

(deftest $-match-child-and-predecessor-miss
  (let [tree [:a {}
              [:b {:class "foo"}
               [:c {}] [:d {:class "foo"}] [:e {} ] [:f {:class "foo"}]]]]
    (is (empty? ($ "b.foo > x ~ .foo" tree)))))

(deftest $-match-descendant-and-predecessor
  (let [tree [:a {}
              [:b {:class "foo"}
               [:c {}] [:d {:class "foo"}] [:e {} ] [:f {:class "foo"}]]]]
    (is (= '(:d :f) (map tag ($ "b.foo c ~ .foo" tree))))))

(deftest $-match-child-and-immediate-predecessor
  (let [tree [:a {}
              [:b {:class "foo"}
               [:c {}] [:d {:class "foo"}] [:e {} ] [:f {:class "foo"}]]]]
    (is (= '([:d {:class "foo"}])
           ($ "b.foo > c + .foo" tree)))))


(deftest $-match-attribute-exists
  (let [tree [:a {} [:b {:x "x" :y ""}] [:c {}]]]
    (is (= '([:b {:x "x" :y ""}])
           ($ "b[x]" tree)))))

(deftest $-match-attribute-exists-miss
  (let [tree [:a {} [:b {:x "x" :y ""}] [:c {}]]]
    (is (empty? ($ "b[z]" tree)))))

(deftest $-match-attribute-exists-multiple
  (let [tree [:a {} [:b {:x "x" :y ""}] [:c {:y "1"}]]]
    (is (= '([:b {:x "x" :y ""}] [:c {:y "1"}])
           ($ "*[y]" tree)))))

(deftest $-match-several-attributes-exist
  (let [tree [:a {} [:b {:x "x" :y ""}] [:c {:y "1"}]]]
    (is (= '([:b {:x "x" :y ""}])
           ($ "*[x y]" tree)))))


(deftest $-match-attribute-value
  (let [tree [:a {} [:b {:x "x" :y ""}] [:c {:y "1"}]]]
    (is (= '([:c {:y "1"}])
           ($ "[y=\"1\"]" tree)))))

(deftest $-match-attribute-value-multiple
  (let [tree [:a {:y "1"} [:b {:x "x" :y ""}] [:c {:y "1"}]]]
    (is (= (concat (list tree) '([:c {:y "1"}]))
           ($ "[y=\"1\"]" tree)))))

(deftest $-match-attribute-value-miss
  (let [tree [:a {} [:b {:x "x" :y ""}] [:c {:y "1"}]]]
    (is (empty? ($ "[z=\"1\"]" tree)))))

(deftest $-match-attribute-value-value-miss
  (let [tree [:a {} [:b {:x "x" :y ""}] [:c {:y "1"}]]]
    (is (empty? ($ "[x=\"1\"]" tree)))))


(deftest $-match-attribute-value-contains-word
  (let [tree [:a {} [:b {:x "foo bar" :y "1"}] [:c {:x "bar baz"}]]]
    (is (= '([:b {:x "foo bar" :y "1"}])
           ($ "[x ~= \"foo\"]" tree)))))

(deftest $-match-attribute-value-contains-word-miss
  (let [tree [:a {} [:b {:x "foo bar" :y "1"}] [:c {:x "bar baz"}]]]
    (is (empty? ($ "[x ~= \"fizz\"]" tree)))))

(deftest $-match-attribute-value-contains-word-attr-miss
  (let [tree [:a {} [:b {:x "foo bar" :y "1"}] [:c {:x "bar baz"}]]]
    (is (empty? ($ "[xx ~= \"foo\"]" tree)))))



