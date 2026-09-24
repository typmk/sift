(ns net.typemark.sift.tests
  (:require [net.typemark.sift.tree :as tree]))

(def ^:private assertions
  #{"is" "are" "expect" "check" "thrown?" "thrown-with-msg?"
    "prop/for-all" "for-all" "prop/for-all*" "checking"})

(defn- assertion-helpers
  [nodes]
  (into #{}
        (for [d (tree/lists-headed-by nodes #{"defn" "defn-"})
              :when (some #(and (= :list (:tag %)) (contains? assertions (:head %)))
                          (tree/children-of nodes d))
              :let [nm (some->> (first (tree/arguments nodes d)) (tree/text nodes))]
              :when nm]
          nm)))

(defn- asserts-inside? [nodes helpers form]
  (some #(and (= :list (:tag %))
              (or (contains? assertions (:head %))
                  (contains? helpers (:head %))))
        (tree/children-of nodes form)))

(defn empty-test
  [nodes]
  (let [helpers (assertion-helpers nodes)]
    (for [n (tree/lists-headed-by nodes #{"deftest" "defspec"})
          :when (not (asserts-inside? nodes helpers n))]
      (tree/hit n "this test asserts nothing, so it passes whatever the code does"))))

(defn testing-without-assertion
  [nodes]
  (let [helpers (assertion-helpers nodes)]
    (for [n (tree/lists-headed-by nodes #{"testing"})
          :when (not (asserts-inside? nodes helpers n))]
      (tree/hit n "this `testing` block contains no assertion"))))

(defn test-with-no-effect
  [nodes]
  (for [n (tree/lists-headed-by nodes #{"is"})
        :let [a (tree/first-argument nodes n)]
        :when (and a (= :list (:tag a))
                   (contains? #{"=" "not=" "==" "<" ">" "<=" ">="} (:head a))
                   (= 1 (count (tree/arguments nodes a))))]
    (tree/hit n (str "(" (:head a) " x) with one operand is always true, so this assertion"
                     " cannot fail"))))
