(ns net.typemark.sift.evidence-test
  "A rule without a rung is a claim, and the suite says so — on the
  registry entry itself, since 2026-08-28; evidence.edn is derived."
  (:require [clojure.test :refer [deftest is testing]]
            [net.typemark.sift :as sift]
            [net.typemark.sift.shape :as shape]
            [net.typemark.sift.prose :as prose]
            [net.typemark.sift.data :as data]
            [net.typemark.sift.typeflow :as typeflow]))

(def rungs #{:compiler :parity :corpus :read :unjudged})

(deftest every-registry-entry-names-a-rung
  (doseq [[r e] (merge shape/rules prose/rules typeflow/rules sift/node-rules-registry)]
    (is (contains? rungs (:evidence e)) (str r " has rung " (:evidence e))))
  (doseq [{:keys [id evidence]} data/rules]
    (is (contains? rungs evidence) (str id " (rules.edn) has rung " evidence))))

(deftest every-rule-names-one-concern
  (doseq [[r e] (sift/evidence)]
    (is (contains? sift/categories (:category e)) (str r " has category " (:category e))))
  (doseq [{:keys [id category]} data/rules]
    (is (contains? sift/categories category) (str id " (rules.edn) has category " category))))

(deftest a-finding-carries-its-registry-category
  (let [fs (:findings (sift/analyze {:text "(ns w (:require [ring.core :as r]))\n(def routes [[\"/pay\" {:post h}]])\n(defn f [a b] (+ a b))" :path "w.clj"}))
        by (into {} (map (juxt :rule :category)) fs)]
    (is (= :security (:csrf-protection-absent by)))
    (is (= :performance (:typeflow/boxed-math by)))))

(deftest the-ledger-is-derived-and-complete
  (let [ledger (sift/evidence)]
    (is (every? (comp rungs :evidence) (vals ledger)))
    (is (= :unjudged (sift/evidence-of :no-such-rule :node)) "an unlisted rule is unjudged")
    (testing "an :unjudged entry is allowed only with a note saying what would judge it"
      (doseq [[r e] ledger :when (= :unjudged (:evidence e))]
        (is (string? (:note e)) (str r " is unjudged and says nothing about why"))))))

(deftest a-finding-carries-its-rung
  (testing "a node rule read on real code"
    (let [fs (:findings (sift/analyze {:text "(ns w (:require [ring.core :as r]))\n(def routes [[\"/pay\" {:post h}]])" :path "w.clj"}))]
      (is (some #(= [:csrf-protection-absent :read] ((juxt :rule :evidence) %)) fs))))
  (testing "a typeflow prediction is compiler-judged when an oracle was given"
    (let [fs (:findings (sift/analyze {:text "(defn f [a b] (+ a b))" :path "x.clj" :classes {:classes {} :by-simple {}}}))]
      (is (some #(= [:typeflow/boxed-math :compiler] ((juxt :rule :evidence) %)) fs))))
  (testing "and without an oracle the same prediction says it was not judged"
    (let [fs (:findings (sift/analyze {:text "(defn f [a b] (+ a b))" :path "x.clj"}))]
      (is (some #(= [:typeflow/boxed-math :unjudged] ((juxt :rule :evidence) %)) fs)))))
