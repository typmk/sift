(ns net.typemark.sift.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.typemark.sift :as sift]
            [net.typemark.sift.registry :as registry]))

(def ^:private everything
  {:rulesets (set (keys registry/rulesets))
   :rules {:doc/ns-missing :warning :doc/narrates-body :warning :doc/docstring :warning :doc/comment :warning
           :tenancy/ambiguous-owner-check {:tenant-pattern "owner"}
           :tenancy/unscoped-tenant-query {:tenant-pattern "owner"}}})

(defn- ids [linter src]
  (set (map :rule (:findings (sift/lint linter [{:path "x.clj" :text src}])))))

(deftest every-built-in-rule-validates
  (let [rules (sift/rules (sift/linter everything))]
    (is (= (count registry/built-in) (count rules)))
    (is (every? (comp registry/rungs :evidence) rules))
    (is (every? (comp registry/checks :extends) rules))
    (is (every? (comp registry/rulesets keyword namespace :id) rules)
        "every built-in rule belongs to a ruleset with a default level")))

(deftest the-default-rulesets-leave-tenancy-off
  (is (not-any? #(= "tenancy" (namespace (:id %))) (sift/rules (sift/linter {})))))

(deftest a-level-turns-a-rule-off-or-changes-it
  (let [src "(defn f [xs] (first (filter even? xs)))"]
    (is (contains? (ids (sift/linter {}) src) :complexity/first-filter-is-some))
    (is (not (contains? (ids (sift/linter {:rules {:complexity/first-filter-is-some :off}}) src)
                        :complexity/first-filter-is-some)))
    (is (= #{:error} (set (map :level (filter #(= :complexity/first-filter-is-some (:rule %))
                                              (:findings (sift/lint (sift/linter {:rules {:complexity/first-filter-is-some :error}})
                                                                    [{:path "x.clj" :text src}])))))))))

(deftest a-rule-named-outside-the-chosen-rulesets-still-runs
  (is (= #{:style/cond-as-case}
         (ids (sift/linter {:rulesets #{} :rules {:style/cond-as-case :info}})
              "(defn f [x] (cond (= x :a) 1 (= x :b) 2 :else 3))"))))

(deftest a-rule-is-defined-by-extending-a-check
  (let [l (sift/linter {:rulesets #{}
                        :rules {:house/no-println {:extends :existence :match '(println ?&_)
                                                   :message "use the logger"}
                                :house/when-not {:extends :substitution :match '(if (not ?t) ?a)
                                                 :emit '(when-not ?t ?a) :applicability :machine-applicable}}})
        fs (:findings (sift/lint l [{:path "x.clj" :text "(defn f [x] (println x) (if (not x) 1))"}]))
        by (group-by :rule fs)]
    (is (= "use the logger" (:message (first (:house/no-println by)))))
    (is (= :warning (:level (first (:house/no-println by)))) "a ruleset sift does not know defaults to warning")
    (is (= '(when-not x 1) (:fix (first (:house/when-not by)))))))

(deftest a-metric-takes-its-maximum-from-config
  (let [src "(defn f [x] (if x (if x (if x 1 2) 3) 4))"
        at (fn [cfg] (filter #(= :complexity/cognitive-complexity (:rule %))
                             (:findings (sift/lint (sift/linter cfg) [{:path "x.clj" :text src}]))))]
    (is (empty? (at {})))
    (is (= [6] (map :cognitive (at {:rules {:complexity/cognitive-complexity {:max 5}}}))))
    (is (seq (filter #(= :house/branches (:rule %))
                     (:findings (sift/lint (sift/linter {:rulesets #{} :rules {:house/branches {:extends :metric :measure :cyclomatic :max 2}}})
                                           [{:path "x.clj" :text src}])))))))

(deftest config-mistakes-are-refused
  (testing "a rule that does not exist"
    (is (thrown? Exception (sift/linter {:rules {:complexity/nonesuch :warning}}))))
  (testing "a check that does not exist"
    (is (thrown? Exception (sift/linter {:rules {:house/x {:extends :regex :match 'x}}}))))
  (testing "a substitution with nothing to substitute"
    (is (thrown? Exception (sift/linter {:rules {:house/x {:extends :substitution :match '(a)}}}))))
  (testing "a level that does not exist"
    (is (thrown? Exception (sift/linter {:rules {:style/cond-as-case :loud}}))))
  (testing "a rule enabled without the parameter it requires"
    (is (thrown? Exception (sift/linter {:rules {:tenancy/unscoped-tenant-query :warning}})))))

(deftest a-rule-without-its-input-is-skipped-and-says-so
  (let [{:keys [skipped]} (sift/lint (sift/linter {}) [{:path "x.clj" :text "(ns x)"}])]
    (is (= :kondo (:missing (first (filter #(= :security/interprocedural-taint (:rule %)) skipped)))))
    (is (= :oracle (:missing (first (filter #(= :performance/boxed-math (:rule %)) skipped)))))
    (is (every? #{:kondo :oracle} (map :missing skipped)))))

(deftest a-finding-carries-its-rung
  (testing "a node rule read on real code"
    (let [fs (:findings (sift/lint (sift/linter {}) [{:path "w.clj" :text "(ns w (:require [ring.core :as r]))\n(def routes [[\"/pay\" {:post h}]])"}]))]
      (is (some #(= [:security/csrf-protection-absent :read] ((juxt :rule :evidence) %)) fs))))
  (testing "a typeflow prediction is compiler-judged when an oracle was given"
    (let [fs (:findings (sift/lint (sift/linter {:oracle {:tags {:classes {} :by-simple {}}}}) [{:path "x.clj" :text "(defn f [a b] (+ a b))"}]))]
      (is (some #(= [:performance/boxed-math :compiler] ((juxt :rule :evidence) %)) fs))))
  (testing "without an oracle the prediction is not made"
    (let [{:keys [findings skipped]} (sift/lint (sift/linter {}) [{:path "x.clj" :text "(defn f [a b] (+ a b))"}])]
      (is (not-any? #(= :performance/boxed-math (:rule %)) findings))
      (is (some #(= :performance/boxed-math (:rule %)) skipped))))
  (testing "an oracle that never compiled the file leaves the prediction unjudged"
    (let [fs (:findings (sift/lint (sift/linter {:oracle {:tags {:classes {} :by-simple {}} :loaded #{"other.clj"}}})
                                   [{:path "x.clj" :text "(defn f [a b] (+ a b))"}]))]
      (is (some #(= [:performance/boxed-math :unjudged] ((juxt :rule :evidence) %)) fs)))))

(deftest min-level-hides-and-fail-level-decides
  (let [src "(defn f [xs] (first (filter even? xs)))\n(defn g [x] (when (re-find #\"a.b\" x) :y))"
        run (fn [cfg] (let [l (sift/linter cfg)] [l (sift/lint l [{:path "x.clj" :text src}])]))
        [l r] (run {})]
    (is (= #{:warning} (set (map :level (:findings r)))))
    (is (not (sift/failed? l r)) "nothing at :error")
    (let [[l r] (run {:fail-level :warning})] (is (sift/failed? l r)))
    (let [[_ r] (run {:min-level :error})] (is (empty? (:findings r))))
    (is (thrown? Exception (sift/linter {:min-level :loud})))
    (is (thrown? Exception (sift/linter {:fail-level :off})))))

(deftest an-unreadable-file-is-an-error-not-a-finding
  (let [{:keys [findings errors]} (sift/lint (sift/linter {}) [{:path "bad.clj" :text "(defn f [x"}])]
    (is (empty? findings))
    (is (= ["bad.clj"] (map :file errors)))))

(deftest test-rules-run-on-test-files-only
  (let [src "(deftest nothing (println :hi))"]
    (is (contains? (set (map :rule (:findings (sift/lint (sift/linter {}) [{:path "test/a_test.clj" :text src}]))))
                   :tests/empty-test))
    (is (not (contains? (set (map :rule (:findings (sift/lint (sift/linter {}) [{:path "src/a.clj" :text src}]))))
                        :tests/empty-test)))))
