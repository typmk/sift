(ns net.typemark.sift.dictionary-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [net.typemark.sift.dictionary :as dictionary]))

(defn- analysis
  [& kws]
  (str "{\"analysis\":{\"keywords\":["
       (str/join
        ","
        (map-indexed
         (fn [i [ns' nm]]
           (str "{\"filename\":\"src/a.clj\",\"row\":" (inc i) ",\"col\":1,"
                "\"end-row\":" (inc i) ",\"end-col\":9"
                (when ns' (str ",\"ns\":\"" ns' "\""))
                ",\"name\":\"" nm "\"}"))
         kws))
       "]}}"))

(def ^:private vocab
  {:dimension ":facet" :custody ":controller" :party/platform-role "an off-graph staff fact"})

(defn- rules-for [& kws]
  (set (map :rule (mapcat val (dictionary/findings (apply analysis kws) vocab)))))

(deftest fires-on-the-callers-table
  (testing "a bare banned term"
    (is (= #{"banned-term"} (rules-for [nil "dimension"])))
    (is (= #{"banned-term"} (rules-for [nil "custody"]))))
  (testing "a namespaced one"
    (is (= #{"banned-term"} (rules-for ["party" "platform-role"])))))

(deftest names-the-replacement
  (let [f (first (mapcat val (dictionary/findings (analysis [nil "custody"]) vocab)))]
    (is (re-find #":custody is banned" (:message f)))
    (is (re-find #":controller" (:message f))
        "the message must say what to use, not only what not to")))

(deftest leaves-everything-else-alone
  (is (empty? (rules-for [nil "facet"] [nil "scope"] [nil "err"] [nil "exit"] [nil "out"])))
  (testing "a same-named keyword in another namespace is a different thing"
    (is (empty? (rules-for ["their" "custody"]))))
  (testing "a bare entry does not match a qualified keyword, nor the reverse"
    (is (empty? (rules-for ["x" "dimension"] [nil "platform-role"])))))

(deftest no-table-no-findings
  (is (= {} (dictionary/findings (analysis [nil "dimension"]) nil)))
  (is (= {} (dictionary/findings (analysis [nil "dimension"]) {}))))

(deftest positions-come-through
  (let [f (first (mapcat val (dictionary/findings (analysis [nil "dimension"]) vocab)))]
    (is (= 1 (:line f)))
    (is (= 1 (:col f)))
    (is (= 9 (:end-col f)))))

(deftest an-empty-or-absent-analysis-yields-nothing
  (is (= {} (dictionary/findings "{\"analysis\":{}}" vocab)))
  (is (= {} (dictionary/findings "{}" vocab))))
