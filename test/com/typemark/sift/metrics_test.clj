(ns com.typemark.sift.metrics-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.typemark.sift.metrics :as metrics]))

(def sample
  (str "(ns example)\n"       "\n"       ";; a comment line\n"       "(defn f [x]\n"       "  (if (pos? x) :a :b))\n"       "(defn g [x] x) ; trailing comment\n"       "(defrecord R [a])\n"))
(deftest counts-lines-the-way-sonar-means-them
  (let [m (metrics/measures sample)]
    (testing "blank and comment-only lines are not code"
      (is (= 5 (:ncloc m))))
    (testing "a line with code and a trailing comment counts once, as code"
      (is (= 1 (:comment-lines m))))))

(deftest counts-the-shapes-sonar-displays
  (let [m (metrics/measures sample)]
    (is (= 2 (:functions m)) "two defn")
    (is (= 1 (:classes m)) "one defrecord")
    (is (= 1 (dec (:complexity m))) "one if, over a base of 1")))

(deftest cognitive-complexity-charges-for-nesting
  (let [flat   (metrics/measures "(defn f [x] (if x 1 2)) (defn g [y] (if y 1 2))")
        nested (metrics/measures "(defn f [x] (if x (if x 1 2) 3))")]
    (testing "two sibling branches cost two"
      (is (= 2 (:cognitive flat))))
    (testing "a branch inside a branch costs more than two siblings do"
      (is (> (:cognitive nested) 2)))
    (testing "while cyclomatic cannot tell them apart"
      (is (= (:complexity flat) (:complexity nested))))))

(deftest a-binding-named-like-a-branch-is-not-a-branch
  (testing "(let [and 1] and) has no decision in it"
    (is (= 1 (:complexity (metrics/measures "(let [and 1] and)")))))
  (testing "but a real and does"
    (is (= 2 (:complexity (metrics/measures "(and a b)"))))))

(deftest comments-inside-strings-are-code
  (let [m (metrics/measures "(def s \"; not a comment\")")]
    (is (= 1 (:ncloc m)))
    (is (= 0 (:comment-lines m)))))

(deftest commented-out-code-is-comment-not-code
  (testing "a #_ form contributes comment lines, not code lines"
    (let [m (metrics/measures "(defn f [] 1)\n#_(defn dead [] 2)\n")]
      (is (= 1 (:ncloc m)))
      (is (= 1 (:comment-lines m)))
      (is (= 1 (:functions m)) "the discarded defn is not a function")))
  (testing "a (comment ...) body is the same"
    (let [m (metrics/measures "(defn f [] 1)\n(comment (if x 1 2))\n")]
      (is (= 1 (:ncloc m)))
      (is (= 1 (:statements m)) "only the live defn")
      (is (= 1 (:complexity m)) "the commented-out if decides nothing"))))

(deftest source-that-does-not-parse-yields-nil-not-zero
  (testing "nil is distinguishable from a genuinely empty file; 0 is not"
    (is (nil? (metrics/measures "(defn f [")))
    (is (some? (metrics/measures "")))))

(defspec measures-are-never-negative 300
  (prop/for-all [s gen/string]
    (let [m (metrics/measures s)]
      (or (nil? m) (every? #(and (integer? %) (>= % 0)) (vals m))))))

(defspec ncloc-never-exceeds-the-line-count 300
  (prop/for-all [s gen/string-alphanumeric]
    (let [m (metrics/measures s)]
      (or (nil? m)
          (<= (:ncloc m) (max 1 (count (str/split-lines (str s "\n")))))))))
