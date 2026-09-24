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

(defn findings [nodes]
  (let [helpers (assertion-helpers nodes)]
   (concat
   (for [n (tree/lists-headed-by nodes #{"deftest" "defspec"})
         :when (not (asserts-inside? nodes helpers n))]
     {:rule "empty-test"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message "this test asserts nothing, so it passes whatever the code does"})

   (for [n (tree/lists-headed-by nodes #{"testing"})
         :when (not (asserts-inside? nodes helpers n))]
     {:rule "testing-without-assertion"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message "this `testing` block contains no assertion"})

   (for [n (tree/lists-headed-by nodes #{"is"})
         :let [a (tree/first-argument nodes n)]
         :when (and a (= :list (:tag a))
                    (contains? #{"=" "not=" "==" "<" ">" "<=" ">="} (:head a))
                    (= 1 (count (tree/arguments nodes a))))]
     {:rule "test-with-no-effect"
      :line (:line n) :col (:col n) :end-line (:end-line n) :end-col (:end-col n)
      :message (str "(" (:head a) " x) with one operand is always true, so this assertion"
                    " cannot fail")}))))
