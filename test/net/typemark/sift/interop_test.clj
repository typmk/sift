(ns net.typemark.sift.interop-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [net.typemark.sift.concurrency :as concurrency]
            [net.typemark.sift.interop :as interop]
            [net.typemark.sift.parse :as parse]))

(defn- nodes [s] (:nodes (parse/parse s)))
(defn- xxe? [s] (boolean (seq (interop/xml-external-entity (nodes s)))))
(defn- trust? [s] (boolean (seq (interop/trust-all-certificates (nodes s)))))
(defn- swap-effect? [s] (boolean (seq (concurrency/side-effect-in-swap (nodes s)))))
(defn- discarded? [s] (boolean (seq (concurrency/discarded-future (nodes s)))))

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
  (is (xxe? (str ns-form "(DocumentBuilderFactory/newInstance)"))))

(deftest does-not-fire-on-the-safe-call
  (testing "commented-out code is not a finding"
    (is (not (xxe? (str ns-form "#_(DocumentBuilderFactory/newInstance)"))))))

(deftest a-hardened-xml-parser-is-not-a-finding
  (testing "an untouched factory is a finding"
    (is (xxe? "(ns a (:import [javax.xml.parsers DocumentBuilderFactory]))
               (defn f [] (DocumentBuilderFactory/newInstance))")))
  (testing "one locked down in the same form is not"
    (doseq [guard ["(.setFeature f \"http://apache.org/xml/features/disallow-doctype-decl\" true)"
                   "(.setFeature f javax.xml.XMLConstants/FEATURE_SECURE_PROCESSING true)"
                   "(.setExpandEntityReferences f false)"]]
      (is (not (xxe? (str "(ns a (:import [javax.xml.parsers DocumentBuilderFactory]))
                           (defn f [] (doto (DocumentBuilderFactory/newInstance) "
                          guard "))")))
          guard))))

(deftest catches-disabled-certificate-validation
  (testing "a hand-written TrustManager exists to switch the check off"
    (is (trust? "(reify javax.net.ssl.X509TrustManager
                   (checkServerTrusted [_ _ _] nil))"))
    (is (trust? "(proxy [javax.net.ssl.HostnameVerifier] []
                   (verify [_ _] true))")))
  (testing "an unrelated reify is not a finding"
    (is (not (trust? "(reify java.lang.Runnable (run [_] nil))")))))

(deftest concurrency-rules-target-what-clojure-actually-gets-wrong
  (testing "a side effect inside a retrying update can happen twice"
    (is (swap-effect? "(swap! a (fn [v] (println v) (inc v)))"))
    (is (swap-effect? "(alter r (fn [v] (send agt f) v))")))
  (testing "a second swap! later on the same line is not inside the first"
    (is (not (swap-effect? "(if f (swap! a assoc k f) (swap! a dissoc k))"))))
  (testing "a pure update is not flagged"
    (is (not (swap-effect? "(swap! a inc)")))
    (is (not (swap-effect? "(swap! a (fn [v] (assoc v :k 1)))"))))
  (testing "a future used as a statement swallows its exception"
    (is (discarded? "(do (future (risky!)) :ok)")))
  (testing "a future whose value is taken is fine"
    (is (not (discarded? "(let [f (future (risky!))] @f)")))))

(defspec interop-never-throws 300
  (prop/for-all [s gen/string]
    (let [{:keys [ok? nodes]} (parse/parse s)]
      (or (not ok?)
          (seqable? (concat (interop/xml-external-entity nodes) (interop/trust-all-certificates nodes)))))))

(defspec concurrency-never-throws 300
  (prop/for-all [s gen/string]
    (let [{:keys [ok? nodes]} (parse/parse s)]
      (or (not ok?)
          (seqable? (doall (concat (concurrency/side-effect-in-swap nodes) (concurrency/discarded-future nodes))))))))
