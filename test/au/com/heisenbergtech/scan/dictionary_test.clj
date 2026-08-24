(ns au.com.heisenbergtech.scan.dictionary-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [au.com.heisenbergtech.scan.dictionary :as dictionary]))

(defn- analysis
  "clj-kondo's keywords analysis, in the shape the real report has."
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

(defn- rules-for [& kws]
  (set (map :rule (mapcat val (dictionary/findings (apply analysis kws))))))

(deftest fires-on-the-project-dictionary
  (testing "a bare banned term"
    (is (= #{"banned-term"} (rules-for [nil "dimension"])))
    (is (= #{"banned-term"} (rules-for [nil "custody"])))
    (is (= #{"banned-term"} (rules-for [nil "visibility"]))))
  (testing "a namespaced one, which is where the operator invariant lives"
    (is (= #{"banned-term"} (rules-for ["party" "platform-role"])))
    (is (= #{"banned-term"} (rules-for ["taxon" "require"])))))

(deftest names-the-replacement
  (let [f (first (mapcat val (dictionary/findings (analysis [nil "custody"]))))]
    (is (re-find #":custody is banned" (:message f)))
    (is (re-find #":controller" (:message f))
        "the message must say what to use, not only what not to")))

(deftest leaves-the-canonical-vocabulary-alone
  (is (empty? (rules-for [nil "facet"] [nil "scope"] ["taxon" "factor"]
                         ["obj" "owner"] [nil "event"] [nil "fault"])))
  (testing "a same-named keyword in another namespace is a different thing"
    (is (empty? (rules-for ["their" "custody"])))))

(deftest the-ambiguous-set-is-deliberate-not-forgotten
  (testing "clojure.java.shell/sh returns {:exit :out :err}; you cannot rename
            another library's contract, and a rule that demands it is ignored"
    (is (empty? (rules-for [nil "err"] [nil "exit"] [nil "out"]))))
  (testing "every ambiguous term is genuinely excluded from the live set"
    (is (empty? (set/intersection (set (keys dictionary/banned))
                                  (set (keys dictionary/ambiguous))))))
  (testing "and the exclusions are recorded, so the gap is visible"
    (is (contains? dictionary/ambiguous [nil "err"]))
    (is (contains? dictionary/ambiguous [nil "audience"])
        "audience is banned by one section of CLAUDE.md and canonical in another")))

(deftest positions-come-through-for-sonar
  (let [f (first (mapcat val (dictionary/findings (analysis [nil "dimension"]))))]
    (is (= 1 (:line f)))
    (is (= 1 (:col f)))
    (is (= 9 (:end-col f)))))

(deftest an-empty-or-absent-analysis-yields-nothing
  (is (= {} (dictionary/findings "{\"analysis\":{}}")))
  (is (= {} (dictionary/findings "{}"))))
