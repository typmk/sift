(ns com.typemark.sift.shape-test
  "The corpus is the spec, as it was in agentia: every file under
  corpus/clear stays silent, every file under corpus/flag fires. Ported
  from places_test.clj with the CLI-only cases (`lint` over dirs, `--apply`)
  dropped — sift never writes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.data :as data]
            [com.typemark.sift.resolve :as resolve]
            [com.typemark.sift.shape :as shape]))

(def ^:private root
  (let [f (io/file (.getPath (io/resource "com/typemark/sift/corpus")))]
    (assert (.isDirectory f) (str "corpus not on classpath: " f))
    f))

(defn- corpus [kind]
  (sort-by str (filter #(re-find #"\.clj[sc]?$" (str %))
                       (file-seq (io/file root kind)))))

(defn- lint [f]
  (shape/findings (slurp f) (str f)))

(defn- at [rel] (io/file root rel))

(deftest control-pair
  (let [a (lint (at "clear/idiomatic.clj"))
        b (lint (at "flag/generated.clj"))]
    (is (empty? a) "house style is silent")
    (is (seq b) "agent shape is not")
    (is (>= (count (filter #(= :place-as-fold (:rule %)) b)) 2)
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
    (is (= 'reduce (first (:counterpart (first fs)))))))

(deftest if-branch-has-no-counterpart
  (let [fs (lint (at "flag/if_branch.clj"))]
    (is (seq fs))
    (is (nil? (:counterpart (first fs))))
    (is (= :unspecified (:applicability (first fs))))))

(deftest loop-map-is-into
  (let [fs (lint (at "flag/loop_map.clj"))]
    (is (seq fs))
    (is (every? #(= :loop-as-map (:rule %)) fs))
    (is (= :machine-applicable (:applicability (first fs))))
    (is (= 'into (first (:counterpart (first fs)))))))

(deftest allow-is-honoured
  (is (empty? (lint (at "clear/allow_meta.clj"))))
  (is (seq (lint (at "flag/plain_accumulator.clj")))))

(deftest every-finding-has-instruction-and-position
  (let [fs (mapcat lint (corpus "flag"))]
    (is (seq fs))
    (is (every? (comp string? :instruction) fs))
    (is (every? (comp pos-int? :line) fs))
    (is (every? shape/rules (map :rule fs)))
    (is (every? shape/applicability (map :applicability fs)))
    (is (every? #{:refactor :readability :warning :design} (map :category fs)))))

(deftest loop-filter-map-is-comp
  (let [fs (lint (at "flag/loop_filter_map.clj"))
        form (:counterpart (first fs))]
    (is (seq fs))
    (is (= :loop-as-map (:rule (first fs))))
    (is (= :machine-applicable (:applicability (first fs))))
    (is (= 'into (first form)))
    (is (= 'comp (first (nth form 2))))))

(deftest generated-is-larger
  (let [a (slurp (at "clear/idiomatic.clj"))
        b (slurp (at "flag/generated.clj"))]
    (is (< (count (str/split-lines a)) (count (str/split-lines b))))))

(deftest unreadable-source-is-silent-not-thrown
  (is (= [] (shape/findings "(defn f [x" "bad.clj"))))

;; ---- resolution: what only clj-kondo can say --------------------------------

(deftest resolution-sees-aliased-core-and-ignores-shadowed
  (let [f    (at "resolved/aliased_core_atom.clj")
        idx  (resolve/index (slurp (at "resolved/aliased_core_atom.analysis.json")))
        bare (shape/findings (slurp f) "aliased_core_atom.clj")
        res  (shape/findings (slurp f) "aliased_core_atom.clj" idx)]
    (testing "without resolution: the aliased atom is missed, the shadowed swap! is flagged"
      (is (= [11] (map :line bare))))
    (testing "with it: the aliased atom is found, the parameter named swap! is not a mutator"
      (is (= [5] (map :line res)))
      (is (= :machine-applicable (:applicability (first res)))))))

;; ---- cond-as-case -----------------------------------------------------------

(deftest cond-over-literals-is-a-case
  (let [fs (lint (at "flag/cond_case.clj"))]
    (is (= [:cond-as-case :cond-as-case] (map :rule fs)))
    (is (every? #(= :machine-applicable (:applicability %)) fs))
    (is (= '(case x :circle "round" :square "boxy" :line "thin" "unknown")
           (:counterpart (first fs))))
    (is (= '(case n 200 :ok 404 :missing nil) (:counterpart (second fs)))
        "no :else -> explicit nil default, because case would throw where cond returns nil")))

(deftest cond-over-vars-is-not-a-case-and-over-nil-is-a-maybe
  (is (empty? (shape/findings "(defn f [x] (cond (= x foo) 1 (= x bar) 2))" "s.clj"))
      "case would read foo as a literal symbol")
  (let [fs (shape/findings "(defn f [x] (cond (= x nil) 1 (= x true) 2))" "s.clj")]
    (is (= 1 (count fs)))
    (is (= :unspecified (:applicability (first fs))))
    (is (nil? (:counterpart (first fs))))))

;; ---- loop-as-reduce ---------------------------------------------------------

(deftest loop-threading-an-accumulator-is-a-reduce
  (let [fs (lint (at "flag/loop_reduce.clj"))]
    (is (= [:loop-as-reduce :loop-as-reduce :loop-as-reduce] (map :rule fs)))
    (is (every? #(= :machine-applicable (:applicability %)) fs))
    (is (= '(reduce (fn [acc x] (+ acc x)) 0 nums) (:counterpart (first fs))))
    (is (= '(reduce (fn [acc r] (assoc acc (:id r) r)) {} rows) (:counterpart (second fs)))
        "binding order reversed, empty? test, let-bound element")
    (is (= '(reduce (fn [best x] (if (> (count x) (count best)) x best)) nil words)
           (:counterpart (nth fs 2))))))

(deftest a-loop-that-is-a-map-is-reported-once
  (let [fs (lint (at "flag/loop_map.clj"))]
    (is (= 1 (count fs)))
    (is (= :loop-as-map (:rule (first fs))))))


;; ---- host boundary ----------------------------------------------------------

(deftest host-escapes-fire-on-the-jvm-file
  (let [fs (lint (at "flag/host_escapes.clj"))
        by (group-by :rule fs)]
    (is (= 2 (count (:catch-all-swallow by))) "nil and :failed; the rethrow and the (ex-info) are not")
    (is (= 2 (count (:mutable-escape by))) "(ArrayList.) and (HashMap. 16)")
    (is (= 1 (count (:reflection-unwarned by))))
    (is (= '(set! *warn-on-reflection* true) (:counterpart (first (:reflection-unwarned by)))))
    (is (every? #{:warning :design} (map :category fs)))))

(deftest js-prop-on-own-object-has-an-aget-counterpart
  (let [fs (lint (at "flag/host_js_prop.cljs"))]
    (is (= [:js-prop-on-own-object :js-prop-on-own-object] (map :rule fs)))
    (is (= '(aget o "ns") (:counterpart (first fs))))
    (is (every? #(= :machine-applicable (:applicability %)) fs))))

(deftest host-clear-files-are-silent
  (is (empty? (lint (at "clear/host_ok.clj"))))
  (is (empty? (lint (at "clear/host_js_ok.cljs")))))

;; ---- rules as data ----------------------------------------------------------

(deftest every-data-rule-fires-once-on-its-flag-line-and-never-on-clear
  (let [ids (set (map :id data/rules))
        fs (filter #(ids (:rule %)) (lint (at "flag/data_rules.clj")))
        by (into {} (map (juxt :rule identity)) fs)]
    (is (= ids (set (map :rule fs))) "every rule in rules.edn fires")
    (is (= (count ids) (count fs)) "and exactly once")
    (is (= '(some (fn [x] (when (even? x) x)) xs) (:counterpart (by :first-filter-is-some))))
    (is (= '(case k :a 1 :b 2 3) (:counterpart (by :cond-literals-with-else-is-case))))
    (is (nil? (:counterpart (by :thread-sleep))))
    (is (= 'a (get-in (by :deref-inside-own-swap) [:binds '?a])) ":inside carried ?a")
    (is (every? shape/applicability (map :applicability fs)))
    (is (every? (comp string? :instruction) fs))
    (is (every? shape/rules (map :rule fs)) "data rules are in the registry"))
  (is (empty? (filter #(contains? (set (map :id data/rules)) (:rule %))
                      (lint (at "clear/data_rules_ok.clj"))))))
