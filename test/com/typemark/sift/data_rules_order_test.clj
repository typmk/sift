(ns com.typemark.sift.data-rules-order-test
  "rules.edn is applied first-match-wins, which is a term-rewriting system
  with no confluence guarantee unless the rules cannot overlap. They cannot:
  every :match / :either pattern starts with a literal symbol, and no head
  is claimed by two rules. That is the property that makes the file's order
  a non-decision, and bin/rule-order is the measurement behind it — 20
  shuffled orders over four corpora, zero forms whose finding moved."
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.data :as data]))

(defn- heads
  "The literal head of every pattern a rule carries. A :head-ns rule has no
  pattern — it names a class and its members are wild — so it contributes
  the class name itself, which is what it claims and what must be unique."
  [{:keys [match either head-ns]}]
  (if head-ns
    [(symbol head-ns)]
    (for [p (if either either [match])
          :let [h (when (seq? p) (first p))]]
      h)))

(deftest every-pattern-has-a-literal-head
  (doseq [r data/rules, h (heads r)]
    (is (and (symbol? h) (not (.startsWith (name h) "?")))
        (str (:id r) " has a pattern whose head is not a literal symbol: " (pr-str h)))))

(deftest no-two-rules-share-a-head
  (testing "a shared head is the only way one form matches two rules"
    (let [owners (reduce (fn [m r] (reduce #(update %1 %2 (fnil conj #{}) (:id r)) m (heads r)))
                         {} data/rules)]
      (doseq [[h ids] owners]
        (is (= 1 (count ids)) (str "head " h " is claimed by " (pr-str ids)))))))
