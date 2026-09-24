(ns net.typemark.sift.security-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [net.typemark.sift.parse :as parse]
            [net.typemark.sift.security :as security]))

(def ^:private rules
  [security/shell-invocation security/hardcoded-credential security/xml-parse security/permissive-file-permissions])

(defn- hits [src]
  (let [{:keys [ok? nodes]} (parse/parse src)]
    (when ok? (vec (mapcat #(% nodes) rules)))))

(defn- credential? [src]
  (boolean (seq (security/hardcoded-credential (:nodes (parse/parse src))))))

(deftest detects-hardcoded-secrets
  (is (credential? "(def api-key \"sk-live-abcdef\")")))

(deftest states-its-own-limits
  (testing "commented-out code is not a finding"
    (is (empty? (hits "#_(def api-key \"sk-live-abcdef\")"))))
  (testing "a short literal is not treated as a credential"
    (is (not (credential? "(def token \"x\")"))))
  (testing "the credential name is matched on segment boundaries"
    (doseq [nm ["*jvm-wide-hostname-bypass-permitted?*" "tokenizer" "compass" "passthrough"]]
      (is (not (credential? (str "(def " nm " \"aaaaaaaaaa\")"))) nm))
    (doseq [nm ["api-key" "db-password" "client-secret" "auth-token"]]
      (is (credential? (str "(def " nm " \"aaaaaaaaaa\")")) nm))))

(defspec never-throws-on-arbitrary-source 300
  (prop/for-all [s gen/string]
    (let [r (hits s)]
      (or (nil? r) (vector? r)))))

(defspec every-finding-has-a-usable-range 300
  (prop/for-all [s gen/string-alphanumeric]
    (every? (fn [f] (and (>= (:line f) 1) (>= (:column f) 1)
                         (> (:end-column f) 0)))
            (or (hits s) []))))
