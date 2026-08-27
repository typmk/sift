(ns com.typemark.sift.shape-test
  "The corpus is the spec, as it was in agentia: every file under
  corpus/clear stays silent, every file under corpus/flag fires. Ported
  from places_test.clj with the CLI-only cases (`lint` over dirs, `--apply`)
  dropped — sift never writes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.resolve :as resolve]
            [com.typemark.sift.shape :as shape]))

(def ^:private root
  (let [f (io/file (.getPath (io/resource "com/typemark/sift/corpus")))]
    (assert (.isDirectory f) (str "corpus not on classpath: " f))
    f))

(defn- corpus [kind]
  (sort-by str (filter #(str/ends-with? (str %) ".clj")
                       (file-seq (io/file root kind)))))

(defn- lint [f]
  (shape/findings (slurp f) (str f)))

(defn- at [rel] (io/file root rel))

(deftest control-pair
  (let [a (lint (at "clear/idiomatic.clj"))
        b (lint (at "flag/generated.clj"))]
    (is (empty? a) "house style is silent")
    (is (seq b) "agent shape is not")
    (is (every? #(= :place-as-fold (:rule %)) b))
    (is (>= (count b) 2) "summarise's result and describe's out/total")))

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
    (is (= :mechanical (:applicability (first fs))))
    (is (= 'reduce (first (:counterpart (first fs)))))))

(deftest if-branch-has-no-counterpart
  (let [fs (lint (at "flag/if_branch.clj"))]
    (is (seq fs))
    (is (nil? (:counterpart (first fs))))
    (is (= :maybe (:applicability (first fs))))))

(deftest loop-map-is-into
  (let [fs (lint (at "flag/loop_map.clj"))]
    (is (seq fs))
    (is (every? #(= :loop-as-map (:rule %)) fs))
    (is (= :mechanical (:applicability (first fs))))
    (is (= 'into (first (:counterpart (first fs)))))))

(deftest allow-is-honoured
  (is (empty? (lint (at "clear/allow_meta.clj"))))
  (is (seq (lint (at "flag/plain_accumulator.clj")))))

(deftest every-finding-has-instruction-and-position
  (let [fs (mapcat lint (corpus "flag"))]
    (is (seq fs))
    (is (every? (comp string? :instruction) fs))
    (is (every? (comp pos-int? :line) fs))
    (is (every? shape/rules (map :rule fs)))))

(deftest loop-filter-map-is-comp
  (let [fs (lint (at "flag/loop_filter_map.clj"))
        form (:counterpart (first fs))]
    (is (seq fs))
    (is (= :loop-as-map (:rule (first fs))))
    (is (= :mechanical (:applicability (first fs))))
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
      (is (= :mechanical (:applicability (first res)))))))

;; ---- cond-as-case -----------------------------------------------------------

(deftest cond-over-literals-is-a-case
  (let [fs (lint (at "flag/cond_case.clj"))]
    (is (= [:cond-as-case :cond-as-case] (map :rule fs)))
    (is (every? #(= :mechanical (:applicability %)) fs))
    (is (= '(case x :circle "round" :square "boxy" :line "thin" "unknown")
           (:counterpart (first fs))))
    (is (= '(case n 200 :ok 404 :missing) (:counterpart (second fs))))))

(deftest cond-over-vars-is-not-a-case-and-over-nil-is-a-maybe
  (is (empty? (shape/findings "(defn f [x] (cond (= x foo) 1 (= x bar) 2))" "s.clj"))
      "case would read foo as a literal symbol")
  (let [fs (shape/findings "(defn f [x] (cond (= x nil) 1 (= x true) 2))" "s.clj")]
    (is (= 1 (count fs)))
    (is (= :maybe (:applicability (first fs))))
    (is (nil? (:counterpart (first fs))))))
