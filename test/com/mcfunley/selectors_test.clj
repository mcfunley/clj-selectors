(ns com.mcfunley.selectors-test
  (:require [clojure.test :refer :all]
            [com.mcfunley.selectors :refer :all]
            [pl.danieljanus.tagsoup]))

(alias 'selectors 'com.mcfunley.selectors)
(alias 'ts 'pl.danieljanus.tagsoup)
(import 'com.mcfunley.selectors.Selector)
(import 'com.mcfunley.selectors.MatchContext)


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


;; ==============================================================================
;; selector?

(deftest selector?-identifies-selector
  (is (selector? (Selector. "foo" '(expr and stuff) identity))))

(deftest selector?-works-nil (is (not (selector? nil))))

(deftest selector?-works-number (is (not (selector? 1))))


;; ==============================================================================
;; MatchContext

(deftest element-works
  (let [el [:foo {}]
        c (make-context el)]
    (is (= (element c) el))))

(deftest children-works
  (let [cs [[:bar {}] [:baz {}]]
        el (make-context (vec `(:foo {} ~@cs)))]
    (is (= (children el) (map #(make-context %1 el el) cs)))))

(deftest no-children
  (let [el (make-context [:foo {}])]
    (is (empty? (children el)))))


(deftest descendant-seq-works
  (let [tree [:foo {} [:bar {}] [:baz {} [:goo {}]]]]
    (is (= '(:bar :baz :goo)
           (map :tag (descendant-seq (make-context tree)))))))

(deftest descendant-seq-no-children
  (is (empty? (descendant-seq (make-context [:foo {}])))))

(deftest context-seq-works
  (let [tree [:a {} [:b {}] [:c {} [:d {}]]]]
    (is (= '(:a :b :c :d) (map :tag (context-seq (make-context tree)))))))

(deftest context-seq-multiple-branches
  (let [tree [:a {}
              [:b {}]
              [:c {} [:d {}]]
              [:e {} [:f {}
                      [:g {}]]]]]
    (is (= '(:a :b :c :d :e :f :g)
           (map :tag (context-seq (make-context tree)))))))



;; ==============================================================================
;; Selector

(deftest selector-invocation
  (is (= '(:foo) ((Selector. "" nil #(list %1)) :foo))))


;; ==============================================================================
;; matchers

(deftest match-element-type-works
  (is ((match-element-type "foo") (make-context [:foo {}]))))

(deftest match-element-type-miss
  (is (not ((match-element-type "foo") (make-context [:bar {}])))))

(deftest match-element-type-returns-element
  (let [element [:foo {}]]
    (is (= ((match-element-type "foo") (make-context element))
           (list element)))))

(deftest match-id-works
  (is ((match-id "bar") (make-context [:foo { :id "bar" }]))))

(deftest match-id-miss
  (is (not ((match-id "baz") (make-context [:foo { :id "bar" }])))))

(deftest match-id-returns-element
  (let [element [:foo { :id "bar" }]]
    (is (= ((match-id "bar") (make-context element)) (list element)))))

(deftest match-id-no-id-attribute
  (is (not ((match-id "baz") (make-context [:foo {}])))))

(deftest match-all-works
  (is (let [expr (match-all
                  (match-element-type "foo")
                  (match-id "bar"))]
        (expr (make-context [:foo { :id "bar" }])))))

(deftest match-all-fail
  (is (not (let [expr (match-all
                       (match-element-type "foo")
                       (match-id "bar"))]
             (expr (make-context [:foo { :id "baz" }]))))))

(deftest match-class-works-one-class
  (is ((match-class "foo") (make-context [:div { :class "foo" }]))))

(deftest match-class-miss
  (is (not ((match-class "foo") (make-context [:div { :class "bar" }])))))

(deftest match-class-many-classes
  (is ((match-class "foo") (make-context [:div { :class "bar foo" }]))))

(deftest match-class-whitespace-significant
  (is (not ((match-class "foo") (make-context [:div { :class "bar foobar" }])))))

(deftest match-class-no-class-attr
  (is (not ((match-class "foo") (make-context [:div {}])))))

(deftest match-class-case-sensitive
  (is (not ((match-class "foo") (make-context [:div { :class "Foo" }])))))

(deftest match-class-returns-element
  (let [element [:div { :class "foo bar baz" }]]
    (is (= ((match-class "bar") (make-context element)) (list element)))))


(deftest match-descendants-one-level
  (let [element [:a {} [:b { :id "x" }]]]
    (is (= '([:b { :id "x" }])
           (match-descendants (match-id "x") (make-context element))))))

(deftest match-descendants-multiple
  (let [element [:a {} [:b { :id "x" }] [:c { :id "x" }]]]
    (is (= '([:b { :id "x" }] [:c { :id "x" }])
           (match-descendants (match-id "x") (make-context element))))))

(deftest match-descendants-two-levels
  (let [element [:a {} [:b { :id "x" } [:c { :id "x" }]]]]
    (is (= '([:b { :id "x" } [ :c { :id "x" }]] [:c { :id "x" }])
           (match-descendants (match-id "x") (make-context element))))))

(deftest match-descendants-many-levels-multiple-branches
  (let [element [:a {}
                 [:b {:id "x"}
                  [:c {:id "x"}]]
                 [:d {}
                  [:e {}
                   [:f {:id "x"}] [:g {:id "x"}]]]]
        matches (match-descendants (match-id "x") (make-context element))]
    
    (is (= '(:b :c :f :g)
           (map first matches)))))

(deftest match-ancestor-works
  (let [expr (match-ancestor (match-element-type "foo") (match-id "goo"))
        tree [:foo {} [:bar {} [:baz { :id "goo" }]]]]
    (is (= (list [:baz { :id "goo" }])
           (expr (make-context tree))))))

(deftest match-ancestor-match-class
  (let [expr (match-ancestor (match-element-type "foo") (match-class "goo"))
        tree [:foo {} [:bar { :class "goo" }]]
        context (make-context tree)]

    (is (= '([:bar { :class "goo" }]) (expr context)))))

(deftest match-ancestor-multiple-matches
  (let [expr (match-ancestor (match-element-type "foo") (match-class "goo"))
        tree [:foo {}
              [:bar { :class "goo" }
               [:baz { :class "goo ball" }]
               [:qux {}]
               [:fizz { :class "boy-named-goo" }]]]
        context (make-context tree)
        result (expr context)
        result-tags (map first result)]

    (is (= '(:bar :baz) result-tags))))

(deftest match-ancestor-multiple-branches
  (let [tree [:a {}
              [:b {:class "x"}
               [:c {:class "x y"}]]
              [:d {}
               [:e {:class "y x z"}]]]
        context (make-context tree)
        expr (match-ancestor (match-element-type "a") (match-class "x"))
        result (expr context)]
    (is (= '(:b :c :e) (map first result)))))


;; ==============================================================================
;; invoking selectors

(deftest invoke-match-all-type-and-class-miss
  (let [s (compile-selector "a.b")]
    (is (= nil (s [:a {} [:b {:class "x"}]])))))
  

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

