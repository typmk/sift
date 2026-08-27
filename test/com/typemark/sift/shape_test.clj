(ns com.typemark.sift.shape-test
  "The corpus is the spec, as it was in agentia: every file under
  corpus/clear stays silent, every file under corpus/flag fires. Ported
  from places_test.clj with the CLI-only cases (`lint` over dirs, `--apply`)
  dropped — sift never writes."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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
    (is (every? #{:place-as-fold :loop-as-map} (map :rule fs)))))

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
