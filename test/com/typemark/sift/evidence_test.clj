(ns com.typemark.sift.evidence-test
  "A rule without an evidence entry is a claim, and the suite says so."
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift :as sift]
            [com.typemark.sift.shape :as shape]
            [com.typemark.sift.prose :as prose]
            [com.typemark.sift.data :as data]
            [com.typemark.sift.typeflow :as typeflow]))

(def registered
  "Every rule id a registry knows."
  (set (concat (keys shape/rules) (keys prose/rules) (map :id data/rules) (keys typeflow/rules)
               [:typeflow/boxed-math :typeflow/reflection :typeflow/uninferred :cognitive-complexity])))

(deftest every-registered-rule-has-an-evidence-entry
  (doseq [r registered]
    (is (contains? (:rules sift/evidence) r) (str r " has no entry in evidence.edn"))))

(deftest every-entry-names-a-rung
  (doseq [[r {:keys [evidence]}] (:rules sift/evidence)]
    (is (contains? #{:compiler :parity :corpus :read :unjudged} evidence) (str r " has rung " evidence))))

(deftest a-finding-carries-its-rung
  (testing "a node rule read on real code"
    (let [fs (:findings (sift/analyze {:text "(ns w (:require [ring.core :as r]))\n(def routes [[\"/pay\" {:post h}]])" :path "w.clj"}))]
      (is (some #(= [:csrf-protection-absent :read] ((juxt :rule :evidence) %)) fs))))
  (testing "a typeflow prediction is compiler-judged"
    (let [fs (:findings (sift/analyze {:text "(defn f [a b] (+ a b))" :path "x.clj"}))]
      (is (some #(= [:typeflow/boxed-math :compiler] ((juxt :rule :evidence) %)) fs)))))
