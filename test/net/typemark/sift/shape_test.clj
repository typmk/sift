(ns net.typemark.sift.shape-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [net.typemark.sift :as sift]
            [net.typemark.sift.registry :as registry]))

(def ^:private root
  (let [f (io/file (.getPath (io/resource "net/typemark/sift/corpus")))]
    (assert (.isDirectory f) (str "corpus not on classpath: " f))
    f))

(defn- corpus [kind]
  (sort-by str (filter #(re-find #"\.clj[sc]?$" (str %))
                       (file-seq (io/file root kind)))))

(def ^:private pattern-ids
  (into #{} (comp (filter #(#{:existence :substitution} (:extends %))) (map :id)) registry/built-in))

(def ^:private shape-ids
  (into pattern-ids
        #{:complexity/place-as-fold :complexity/loop-as-map :complexity/loop-as-reduce :style/cond-as-case
          :style/cond-as-build-up :style/let-as-thread
          :suspicious/catch-all-swallow :suspicious/mutable-escape
          :correctness/js-prop-on-own-object}))

(defn- linter
  ([] (linter nil))
  ([kondo] (sift/linter (cond-> {:rulesets #{} :rules (zipmap shape-ids (repeat :warning))}
                          kondo (assoc :kondo kondo)))))

(defn- lint-text
  ([text path] (lint-text text path (linter)))
  ([text path l] (:findings (sift/lint l [{:path path :text text}]))))

(defn- lint [f]
  (lint-text (slurp f) (.getName f)))

(defn- at [rel] (io/file root rel))

(deftest control-pair
  (let [a (lint (at "clear/idiomatic.clj"))
        b (lint (at "flag/generated.clj"))]
    (is (empty? a) "house style is silent")
    (is (seq b) "agent shape is not")
    (is (>= (count (filter #(= :complexity/place-as-fold (:rule %)) b)) 2)
        "summarise's result and describe's out/total")))

(deftest flag-dir
  (doseq [f (corpus "flag")]
    (testing (.getName f)
      (is (seq (lint f))))))

(deftest clear-dir
  (doseq [f (corpus "clear")]
    (testing (.getName f)
      (is (empty? (lint f))))))

(deftest counterpart-is-reduce
  (let [fs (lint (at "flag/atom_accumulator.clj"))]
    (is (= 1 (count fs)))
    (is (= :machine-applicable (:applicability (first fs))))
    (is (= 'reduce (first (:fix (first fs)))))))

(deftest if-branch-has-no-counterpart
  (let [fs (lint (at "flag/if_branch.clj"))]
    (is (seq fs))
    (is (nil? (:fix (first fs))))
    (is (= :unspecified (:applicability (first fs))))))

(deftest loop-map-is-into
  (let [fs (lint (at "flag/loop_map.clj"))]
    (is (seq fs))
    (is (every? #(= :complexity/loop-as-map (:rule %)) fs))
    (is (= :machine-applicable (:applicability (first fs))))
    (is (= 'into (first (:fix (first fs)))))))

(deftest allow-is-honoured
  (is (empty? (lint (at "clear/allow_meta.clj"))))
  (is (seq (lint (at "flag/plain_accumulator.clj")))))

(deftest every-finding-has-instruction-and-position
  (let [fs (mapcat lint (corpus "flag"))]
    (is (seq fs))
    (is (every? (comp string? :instruction) fs))
    (is (every? (comp pos-int? :line) fs))
    (is (every? shape-ids (map :rule fs)))
    (is (every? registry/applicability (map :applicability fs)))))

(deftest loop-filter-map-is-comp
  (let [fs (lint (at "flag/loop_filter_map.clj"))
        form (:fix (first fs))]
    (is (seq fs))
    (is (= :complexity/loop-as-map (:rule (first fs))))
    (is (= :machine-applicable (:applicability (first fs))))
    (is (= 'into (first form)))
    (is (= 'comp (first (nth form 2))))))

(deftest generated-is-larger
  (let [a (slurp (at "clear/idiomatic.clj"))
        b (slurp (at "flag/generated.clj"))]
    (is (< (count (str/split-lines a)) (count (str/split-lines b))))))

(deftest unreadable-source-is-silent-not-thrown
  (is (= [] (lint-text "(defn f [x" "bad.clj"))))

(deftest resolution-sees-aliased-core-and-ignores-shadowed
  (let [f    (at "resolved/aliased_core_atom.clj")
        bare (lint-text (slurp f) "aliased_core_atom.clj")
        res  (lint-text (slurp f) "aliased_core_atom.clj" (linter (slurp (at "resolved/aliased_core_atom.analysis.json"))))]
    (testing "without resolution: the aliased atom is missed, the shadowed swap! is flagged"
      (is (= [11] (map :line bare))))
    (testing "with it: the aliased atom is found, the parameter named swap! is not a mutator"
      (is (= [5] (map :line res)))
      (is (= :machine-applicable (:applicability (first res)))))))

(deftest cond-over-literals-is-a-case
  (let [fs (lint (at "flag/cond_case.clj"))]
    (is (= [:style/cond-as-case :style/cond-as-case] (map :rule fs)))
    (is (every? #(= :machine-applicable (:applicability %)) fs))
    (is (= '(case x :circle "round" :square "boxy" :line "thin" "unknown")
           (:fix (first fs))))
    (is (= '(case n 200 :ok 404 :missing nil) (:fix (second fs)))
        "no :else -> explicit nil default, because case would throw where cond returns nil")))

(deftest cond-over-vars-is-not-a-case-and-over-nil-is-a-maybe
  (is (empty? (lint-text "(defn f [x] (cond (= x foo) 1 (= x bar) 2))" "s.clj"))
      "case would read foo as a literal symbol")
  (let [fs (lint-text "(defn f [x] (cond (= x nil) 1 (= x true) 2))" "s.clj")]
    (is (= 1 (count fs)))
    (is (= :unspecified (:applicability (first fs))))
    (is (nil? (:fix (first fs))))))

(deftest loop-threading-an-accumulator-is-a-reduce
  (let [fs (lint (at "flag/loop_reduce.clj"))]
    (is (= [:complexity/loop-as-reduce :complexity/loop-as-reduce :complexity/loop-as-reduce] (map :rule fs)))
    (is (every? #(= :machine-applicable (:applicability %)) fs))
    (is (= '(reduce (fn [acc x] (+ acc x)) 0 nums) (:fix (first fs))))
    (is (= '(reduce (fn [acc r] (assoc acc (:id r) r)) {} rows) (:fix (second fs)))
        "binding order reversed, empty? test, let-bound element")
    (is (= '(reduce (fn [best x] (if (> (count x) (count best)) x best)) nil words)
           (:fix (nth fs 2))))))

(deftest a-loop-that-is-a-map-is-reported-once
  (let [fs (lint (at "flag/loop_map.clj"))]
    (is (= 1 (count fs)))
    (is (= :complexity/loop-as-map (:rule (first fs))))))

(deftest host-escapes-fire-on-the-jvm-file
  (let [fs (lint (at "flag/host_escapes.clj"))
        by (group-by :rule fs)]
    (is (= 2 (count (:suspicious/catch-all-swallow by))) "nil and :failed; the rethrow and the (ex-info) are not")
    (is (= 2 (count (:suspicious/mutable-escape by))) "(ArrayList.) and (HashMap. 16)")
    (is (every? #{:warning} (map :level fs)))))

(deftest js-prop-on-own-object-has-an-aget-counterpart
  (let [fs (lint (at "flag/host_js_prop.cljs"))]
    (is (= [:correctness/js-prop-on-own-object :correctness/js-prop-on-own-object] (map :rule fs)))
    (is (= '(aget o "ns") (:fix (first fs))))
    (is (every? #(= :machine-applicable (:applicability %)) fs))))

(deftest host-clear-files-are-silent
  (is (empty? (lint (at "clear/host_ok.clj"))))
  (is (empty? (lint (at "clear/host_js_ok.cljs")))))

(deftest every-pattern-rule-fires-once-on-its-flag-line-and-never-on-clear
  (let [fs (filter #(pattern-ids (:rule %)) (lint (at "flag/data_rules.clj")))
        by (into {} (map (juxt :rule identity)) fs)]
    (is (= pattern-ids (set (map :rule fs))) "every pattern rule fires")
    (is (= (count pattern-ids) (count fs)) "and exactly once")
    (is (= '(some (fn [x] (when (even? x) x)) xs) (:fix (by :complexity/first-filter-is-some))))
    (is (nil? (:fix (by :performance/thread-sleep))))
    (is (every? registry/applicability (map :applicability fs)))
    (is (every? (comp string? :instruction) fs)))
  (is (empty? (filter #(pattern-ids (:rule %)) (lint (at "clear/data_rules_ok.clj"))))))
