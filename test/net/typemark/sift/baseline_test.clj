(ns net.typemark.sift.baseline-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.typemark.sift.baseline :as b]))

(def ^:private f1 {:file "a.clj" :rule :place-as-fold :symbol 'acc :line 10})
(def ^:private f2 {:file "a.clj" :rule :place-as-fold :symbol 'acc :line 40})
(def ^:private f3 {:file "b.clj" :rule :loop-as-map :symbol 'xs :line 3})

(deftest a-baseline-hides-what-it-holds-and-nothing-more
  (let [base (b/counts [f1 f3])]
    (testing "the same finding on a different line is the same finding"
      (is (= [] (b/new-findings base [(assoc f1 :line 99) f3]))))
    (testing "a second acc in the same file is new; the first is known"
      (is (= [f2] (b/new-findings base [f1 f2 f3]))))
    (testing "an empty baseline reports everything"
      (is (= [f1 f2 f3] (b/new-findings {} [f1 f2 f3]))))))

(deftest render-and-parse-round-trip
  (let [base (b/counts [f1 f2 f3])
        text (b/render base)]
    (is (= base (b/parse text)))
    (testing "sorted, one entry per line, so a diff names the moved finding"
      (is (< (.indexOf text "a.clj") (.indexOf text "b.clj"))))))

(deftest a-line-that-does-not-read-is-an-error
  (is (thrown? Exception (b/parse "[[\"a.clj\" :r \"x\"] 1]\nnot edn ]"))))
