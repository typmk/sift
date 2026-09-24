(ns net.typemark.sift.data-rules-order-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.typemark.sift.data :as data]))

(defn- heads
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
