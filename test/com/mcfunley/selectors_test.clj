(ns com.mcfunley.selectors-test
  (:require [clojure.test :refer :all]
            [com.mcfunley.selectors :refer :all]))

(alias 'selectors 'com.mcfunley.selectors)

;; ==============================================================================
;; compilation tests

(deftest compile-any
  (is (= `(selectors/match-element-type :any)
         (compile-selector "*"))))

(deftest compile-element-type
  (is (= `(selectors/match-element-type "foo")
         (compile-selector "foo"))))

(deftest compile-with-child
  (is (= `(selectors/match-with-child
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (compile-selector "foo > bar"))))

(deftest compile-immediately-preceding
  (is (= `(selectors/match-immediately-preceding
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (compile-selector "foo + bar"))))

(deftest compile-preceding
  (is (= `(selectors/match-preceding
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (compile-selector "foo ~ bar"))))

(deftest compile-extra-whitespace
  (is (= `(selectors/match-preceding
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (compile-selector "foo   ~ bar "))))

(deftest compile-ancestor
  (is (= `(selectors/match-ancestor
           (selectors/match-element-type "foo")
           (selectors/match-element-type "bar"))
         (compile-selector "foo bar"))))

(deftest compile-ancestor-class-match
  (is (= `(selectors/match-ancestor
           (selectors/match-element-type "foo")
           (selectors/match-class "bar"))
         (compile-selector "foo .bar"))))

(deftest compile-type-and-id
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-id "bar")))
         (compile-selector "foo#bar"))))

(deftest compile-type-id-and-class
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-id "bar")
            (selectors/match-class "baz")))
         (compile-selector "foo#bar.baz"))))

(deftest compile-any-with-class
  (is (= `(selectors/match-all
           ((selectors/match-element-type :any)
            (selectors/match-class "foo")))
         (compile-selector "*.foo"))))

(deftest compile-first-line
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/select-first-line))))
      (compile-selector "foo::first-line")))

(deftest compile-first-letter
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-class "bar")
            (selectors/select-first-letter))))
      (compile-selector "foo.bar::first-letter")))


(deftest compile-having-attribute
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-attribute-exists "bar")))
         (compile-selector "foo[bar]"))))

(deftest compile-having-two-attributes
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-attribute-exists "bar")
            (selectors/match-attribute-exists "baz")))
         (compile-selector "foo[bar baz]"))))

(deftest compile-attribute-contains-word
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-attribute-value-contains-word "bar" "baz")))
         (compile-selector "foo[bar ~= \"baz\"]"))))

(deftest compile-attribute-value-begins
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-class "bar")
            (selectors/match-attribute-value-begins "baz" "fizz buzz")))
         (compile-selector "foo.bar[baz^=\"fizz buzz\"]"))))

(deftest compile-attribute-value-ends
  (is (= `(selectors/match-all
           ((selectors/match-element-type :any)
            (selectors/match-attribute-value-ends "foo" "bar")))
         (compile-selector "*[foo $= \"bar\"]"))))

(deftest compile-attribute-value-contains-pattern
  (is (= `(selectors/match-all
           ((selectors/match-element-type :any)
            (selectors/match-id "crap")
            (selectors/match-attribute-value-contains-pattern "foo" "bar")))
         (compile-selector "*#crap[foo *=   \"bar\"]"))))

(deftest compile-wacky-attribute-value
  (is (= `(selectors/match-all
           ((selectors/match-class "foo")
            (selectors/match-attribute-exists "bar")
            (selectors/match-attribute-value-lang-subcode "baz" "en")
            (selectors/match-attribute-exists "goo")))
         (compile-selector ".foo[bar baz|=\"en\" goo  ]"))))

(deftest compile-match-attribute-value
  (is (= `(selectors/match-all
           ((selectors/match-id "foo")
            (selectors/match-attribute-exists "bar")
            (selectors/match-attribute-value "baz" "goo ball")
            (selectors/match-attribute-value-contains-pattern "qux" "quick")))
         (compile-selector "#foo[bar baz=\"goo ball\" qux *= \"quick\"]"))))

(deftest compile-complicated-first-line
  (is (= `(selectors/match-ancestor
           (selectors/match-all
            ((selectors/match-id "foo")
             (selectors/match-attribute-value "bar" "baz")))
           (selectors/match-all
            ((selectors/match-class "qux")
             (selectors/select-first-line))))
         (compile-selector "#foo[bar=\"baz\"] .qux::first-line"))))

(deftest compile-first-line
  (is (= `(selectors/match-all
           ((selectors/match-class "foo")
            (selectors/select-first-line)))
         (compile-selector ".foo::first-line"))))

(deftest compile-first-letter
  (is (= `(selectors/match-with-child
           (selectors/match-element-type "foo")
           (selectors/match-all
            ((selectors/match-id "bar")
             (selectors/select-first-letter))))
         (compile-selector "foo > #bar::first-letter"))))

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
           ((selectors/match-attribute-exists "foo")
            (selectors/match-all
             ((selectors/match-attribute-exists "bar")))))
         (parse-attribute-selector "[foo bar]"))))

(deftest parse-wacky-attribute-selector
  (is (= `(selectors/match-all
           ((selectors/match-attribute-exists "bar")
            (selectors/match-all
             ((selectors/match-attribute-value-lang-subcode "baz" "en")
              (selectors/match-all
               ((selectors/match-attribute-exists "goo"))))))
         (parse-attribute-selector "[bar baz|=\"en\" goo  ]")))))


;; ==============================================================================
;; simplification

(deftest flatten-match-all-nop
  (is (= '(com.mcfunley.selectors/match-all ((foo bar)))
         (flatten-match-all
          '(com.mcfunley.selectors/match-all ((foo bar)))
          ))))

(deftest flatten-match-all-one-level
  (is (= '(com.mcfunley.selectors/match-all
           ((foo bar)
            (baz goo)
            (fizz buzz)
            (qux quick)))

         (flatten-match-all
          '(com.mcfunley.selectors/match-all
            ((foo bar)
             (com.mcfunley.selectors/match-all
              ((baz goo)
               (fizz buzz)))
             (qux quick)))))))

(deftest flatten-match-all-two-levels
  (is (= '(com.mcfunley.selectors/match-all
           ((foo bar)
            (baz goo)
            (fizz buzz)
            (qux quick)
            (quack quack)))

         (flatten-match-all
          '(com.mcfunley.selectors/match-all
            ((foo bar)
             
             (com.mcfunley.selectors/match-all
              ((com.mcfunley.selectors/match-all ((baz goo)))
               (com.mcfunley.selectors/match-all ((fizz buzz)))
               (qux quick)))

             (com.mcfunley.selectors/match-all ((quack quack)))))))))


(deftest simplify-simple
  (is (= '(foo (bar baz))
         (simplify '(foo (bar baz))))))

(deftest simplify-match-all
  (is (= '(com.mcfunley.selectors/match-all
           ((foo bar)
            (baz goo)))

         (simplify
          '(com.mcfunley.selectors/match-all
            ((foo bar)
             (com.mcfunley.selectors/match-all ((baz goo)))))))))

(deftest simplify-buried-match-alls
  (is (= '(foo
           (bar (com.mcfunley.selectors/match-all ((baz goo) (fizz buzz))))
           (bar (quack quack)))

         (simplify
          '(foo
            (bar (com.mcfunley.selectors/match-all
                  ((com.mcfunley.selectors/match-all ((baz goo)))
                   (fizz buzz))))
            (bar (quack quack)))))))

(deftest simplify-single-select
  (is (= '(com.mcfunley.selectors/select-first-line)
         (simplify '(com.mcfunley.selectors/select-first-line)))))
