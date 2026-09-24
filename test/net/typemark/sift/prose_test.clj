(ns net.typemark.sift.prose-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [net.typemark.sift.prose :as prose]))

(def ^:private fs
  (get (prose/findings (slurp (io/resource "net/typemark/sift/corpus/prose/docs.analysis.json")))
       "docs.clj"))

(defn- rules-on [sym] (set (map :rule (filter #(= sym (:symbol %)) fs))))

(deftest the-generated-shapes-fire
  (is (= #{:doc/restates-name :doc/params-unnamed} (rules-on "parse-config"))
      "\"Parses the config.\" on parse-config, naming neither path nor opts")
  (is (= #{:doc/hedge} (rules-on "total-of")) "this function … is used to … basically … simply")
  (is (= #{:doc/placeholder} (rules-on "tidy")))
  (is (= #{:doc/ns-missing} (rules-on "docs")) "\"Utilities.\" is one word"))

(deftest the-hand-written-shape-is-silent
  (is (empty? (rules-on "well-documented")))
  (is (empty? (rules-on "undocumented")) "no docstring is clj-kondo's finding, not a prose one"))

(deftest every-finding-is-placed-and-categorised
  (is (every? (comp pos-int? :line) fs))
  (is (every? #{:documentation} (map :category fs)))
  (is (every? (comp string? :instruction) fs)))
