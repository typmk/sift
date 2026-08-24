(ns com.typemark.sift.security-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.typemark.sift.security :as security]))

(defn- rules-for [src] (set (map :rule (security/findings-of-source src))))

(deftest detects-hardcoded-secrets
  (is (contains? (rules-for "(def api-key \"sk-live-abcdef\")") "hardcoded-credential")))

(deftest states-its-own-limits
  (testing "commented-out code is not a finding"
    (is (empty? (rules-for "#_(def api-key \"sk-live-abcdef\")"))))
  (testing "a short literal is not treated as a credential"
    (is (not (contains? (rules-for "(def token \"x\")") "hardcoded-credential"))))
  (testing "the credential name is matched on segment boundaries -- `bypass`
            is not `pass`, and a rule that cries wolf gets ignored"
    (doseq [nm ["*jvm-wide-hostname-bypass-permitted?*" "tokenizer" "compass" "passthrough"]]
      (is (not (contains? (rules-for (str "(def " nm " \"aaaaaaaaaa\")")) "hardcoded-credential"))
          nm))
    (doseq [nm ["api-key" "db-password" "client-secret" "auth-token"]]
      (is (contains? (rules-for (str "(def " nm " \"aaaaaaaaaa\")")) "hardcoded-credential")
          nm))))

(defspec never-throws-on-arbitrary-source 300
  (prop/for-all [s gen/string]
    (let [r (security/findings-of-source s)]
      (or (nil? r) (vector? r)))))

(defspec every-finding-has-a-usable-range 300
  (prop/for-all [s gen/string-alphanumeric]
    (every? (fn [f] (and (>= (:line f) 1) (>= (:col f) 1)
                         (> (:end-col f) 0)))
            (or (security/findings-of-source s) []))))
