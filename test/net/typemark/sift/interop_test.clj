(ns net.typemark.sift.interop-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [net.typemark.sift.concurrency :as concurrency]
            [net.typemark.sift.interop :as interop]
            [net.typemark.sift.parse :as parse]))

(defn- nodes [s] (:nodes (parse/parse s)))
(defn- rules-for [s] (set (map :rule (interop/all-findings (nodes s)))))
(defn- conc-for [s] (set (map :rule (concurrency/findings (nodes s)))))

(def ^:private ns-form
  "(ns app
     (:import [java.security MessageDigest]
              [javax.crypto Cipher]
              [javax.net.ssl SSLContext]
              [javax.xml.parsers DocumentBuilderFactory]
              java.util.Random
              java.io.ObjectInputStream))\n")

(deftest resolves-imports-both-ways
  (let [i (interop/imports (nodes ns-form))]
    (testing "[package Class Class] form"
      (is (= "java.security.MessageDigest" (get i "MessageDigest")))
      (is (= "javax.crypto.Cipher" (get i "Cipher"))))
    (testing "bare fully-qualified symbol"
      (is (= "java.util.Random" (get i "Random")))
      (is (= "java.io.ObjectInputStream" (get i "ObjectInputStream"))))))

(deftest catches-the-jdk-misuse-clojure-inherits
    (is (contains? (rules-for (str ns-form "(DocumentBuilderFactory/newInstance)"))
                 "xml-external-entity"))
  )

(deftest does-not-fire-on-the-safe-call
  (testing "commented-out code is not a finding"
    (is (empty? (rules-for (str ns-form "#_(ObjectInputStream. in)"))))))

(deftest a-hardened-xml-parser-is-not-a-finding
  (testing "an untouched factory is a finding"
    (is (contains? (rules-for "(ns a (:import [javax.xml.parsers DocumentBuilderFactory]))
                               (defn f [] (DocumentBuilderFactory/newInstance))")
                   "xml-external-entity")))
  (testing "one locked down in the same form is not"
    (doseq [guard ["(.setFeature f \"http://apache.org/xml/features/disallow-doctype-decl\" true)"
                   "(.setFeature f javax.xml.XMLConstants/FEATURE_SECURE_PROCESSING true)"
                   "(.setExpandEntityReferences f false)"]]
      (is (empty? (rules-for (str "(ns a (:import [javax.xml.parsers DocumentBuilderFactory]))
                                   (defn f [] (doto (DocumentBuilderFactory/newInstance) "
                                  guard "))")))
          guard))))

(deftest catches-disabled-certificate-validation
  (testing "a hand-written TrustManager exists to switch the check off"
    (is (contains? (rules-for "(reify javax.net.ssl.X509TrustManager
                                 (checkServerTrusted [_ _ _] nil))")
                   "trust-all-certificates"))
    (is (contains? (rules-for "(proxy [javax.net.ssl.HostnameVerifier] []
                                 (verify [_ _] true))")
                   "trust-all-certificates")))
  (testing "an unrelated reify is not a finding"
    (is (empty? (rules-for "(reify java.lang.Runnable (run [_] nil))")))))

(deftest concurrency-rules-target-what-clojure-actually-gets-wrong
  (testing "a side effect inside a retrying update can happen twice"
    (is (contains? (conc-for "(swap! a (fn [v] (println v) (inc v)))") "side-effect-in-swap"))
    (is (contains? (conc-for "(alter r (fn [v] (send agt f) v))") "side-effect-in-swap")))
  (testing "a pure update is not flagged -- that is the whole point of swap!"
    (is (empty? (conc-for "(swap! a inc)")))
    (is (empty? (conc-for "(swap! a (fn [v] (assoc v :k 1)))"))))
  (testing "a second swap! later on the same line is not inside the first"
    (is (empty? (conc-for "(if f (swap! a assoc k f) (swap! a dissoc k))"))))
  (testing "a future used as a statement swallows its exception"
    (is (contains? (conc-for "(do (future (risky!)) :ok)") "discarded-future")))
  (testing "a future whose value is taken is fine"
    (is (empty? (conc-for "(let [f (future (risky!))] @f)")))))

(defspec interop-never-throws 300
  (prop/for-all [s gen/string]
    (let [{:keys [ok? nodes]} (parse/parse s)]
      (or (not ok?) (seq? (interop/all-findings nodes)) (vector? (interop/all-findings nodes))))))

(defspec concurrency-never-throws 300
  (prop/for-all [s gen/string]
    (let [{:keys [ok? nodes]} (parse/parse s)]
      (or (not ok?) (some? (seq (concurrency/findings nodes))) true))))
