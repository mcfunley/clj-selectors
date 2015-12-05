(ns com.mcfunley.selectors-test
  (:require [clojure.test :refer :all]
            [com.mcfunley.selectors :refer :all]))

(alias 'selectors 'com.mcfunley.selectors)

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
           ((selectors/match-element-type "foo")
            (selectors/match-id "bar")))
         (parse-selector "foo#bar"))))

(deftest parse-type-id-and-class
  (is (= `(selectors/match-all
           ((selectors/match-element-type "foo")
            (selectors/match-id "bar")
            (selectors/match-class "baz")))
         (parse-selector "foo#bar.baz"))))

;; (deftest parse-having-attribute
;;   (is (= `(every? (selectors/match-element-type "foo")
;;                   (selectors/match-attribute "bar"))
;;          (parse-selector "foo[bar]"))))


(deftest tokenize-filter-exp
  (is (= (tokenize-element "foo[bar]")
         '("foo" "[bar]"))))

(deftest tokenize-id-and-class
  (is (= (tokenize-element "foo.bar#baz")
         '("foo" ".bar" "#baz"))))

(deftest tokenize-class-and-id
  (is (= (tokenize-element "foo#bar.baz")
         '("foo" "#bar" ".baz"))))

(deftest tokenize-class-id-filter-exp
  (is (= (tokenize-element "foo[bar].baz#goo")
         '("foo" "[bar]" ".baz" "#goo"))))
