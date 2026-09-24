(ns net.typemark.sift.node-rules-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.typemark.sift :as sift]
            [net.typemark.sift.concurrency :as concurrency]
            [net.typemark.sift.interop :as interop]
            [net.typemark.sift.parse :as parse]
            [net.typemark.sift.regex :as regex]
            [net.typemark.sift.security :as security]
            [net.typemark.sift.tenancy :as tenancy]
            [net.typemark.sift.tests :as tests]
            [net.typemark.sift.web :as web]))

(defn- hits [f src] (seq (f (:nodes (parse/parse src)))))

(defn- fires
  [f bad good]
  (is (hits f bad) (str "missed: " (pr-str bad)))
  (is (not (hits f good)) (str "false positive on: " (pr-str good))))

(def ^:private tenant #"(?i)(^|[-_*/:.])(owner|tenant)([-_*?!/]|$)")

(def ^:private shared #{"taxon"})

(deftest ambiguous-owner-check
  (testing "nil owner means both untenanted and unresolved, and a two-valued test sends both down one branch"
    (fires #(tenancy/ambiguous-owner-check % tenant)
           "(defn h [o] (if (nil? (:obj/owner o)) (throw (ex-info \"no\" {})) :ok))"
           "(defn h [o] (case (owner-of o) ::unresolved (throw (ex-info \"no\" {})) :ok))")))

(deftest unscoped-tenant-query
  (let [f #(tenancy/unscoped-tenant-query % tenant shared)]
    (fires f
           "(d/q '[:find ?e :where [?e :product/sku]] db)"
           "(d/q '[:find ?e :in $ ?owner :where [?e :obj/owner ?owner]] db o)")
    (testing "a pull takes an entity id the caller already holds"
      (is (not (hits f "(d/pull db '[*] eid)"))))
    (testing "the schema has no tenant to name"
      (is (not (hits f "(d/q '[:find ?e :where [?e :db/ident _]] db)"))))
    (testing "nor does a project's shared layer, once the caller names it"
      (let [q "(d/q '[:find ?e :where [?e :taxon/factor _]] db)"]
        (is (hits #(tenancy/unscoped-tenant-query % tenant #{}) q))
        (is (not (hits f q)))))
    (testing "a query naming no attribute says nothing either way; the safer reading keeps it"
      (is (hits f "(d/q query db)")))))

(deftest tenancy-through-the-linter
  (let [l (sift/linter {:rulesets #{} :rules {:tenancy/unscoped-tenant-query {:tenant-pattern "owner"}}})]
    (is (= [:tenancy/unscoped-tenant-query]
           (map :rule (:findings (sift/lint l [{:path "x.clj" :text "(d/q query db)"}])))))))

(deftest redos-vulnerable-regex
  (testing "brace-nested quantifiers, the shape that actually backtracks"
    (fires regex/redos-vulnerable-regex
           "(def p #\"(a{1,9}){1,9}\")"
           "(def p #\"^[a-z]+$\")")))

(deftest partial-match-validation
  (testing "re-find succeeds on a PARTIAL match; an unanchored pattern in a decision position accepts anything containing one"
    (fires regex/partial-match-validation
           "(defn ok? [s] (when (re-find #\"good\\\\.example\" s) :yes))"
           "(defn ok? [s] (when (re-find #\"^good$\" s) :yes))")))

(deftest xss-unescaped-output
  (fires web/xss-unescaped-output
         "(h/raw (str \"<b>\" x \"</b>\"))"
         "(h/raw \"<hr>\")"))

(deftest csrf-protection-absent
  (fires web/csrf-protection-absent
         "(ns app.routes (:require [reitit.ring :as ring]))\n(def routes [[\"/pay\" {:post handler}]])"
         (str "(ns app.routes (:require [reitit.ring :as ring]"
              " [ring.middleware.anti-forgery :as af]))\n"
              "(def routes [[\"/pay\" {:post handler}]])"))
  (testing "a method keyword as an argument or a dispatch-table key is not a route"
    (is (not (hits web/csrf-protection-absent "(ns app.routes (:require [reitit.ring :as ring]))\n(defn f [db] (crud-decision db :delete 1))")))
    (is (not (hits web/csrf-protection-absent "(ns app.routes (:require [reitit.ring :as ring]))\n(def ops {:delete (partial delete-op conn)})")))))

(deftest sensitive-data-logged
  (fires web/sensitive-data-logged
         "(log/info \"tok\" auth-token)"
         "(log/info \"count\" n)"))

(deftest cookie-missing-security-flags
  (fires web/cookie-missing-security-flags
         "(def c {:cookies {\"sid\" {:value v :max-age 3600}}})"
         "(def c {:cookies {\"sid\" {:value v :max-age 3600 :http-only true :secure true}}})"))

(deftest empty-test
  (fires tests/empty-test
         "(deftest nothing (println :hi))"
         "(deftest something (is (= 1 1)))")
  (testing "a deftest delegating to a helper that asserts is not empty; one delegating to a helper that does not, is"
    (is (not (hits tests/empty-test "(defn- fires [a b] (is (= a b)))\n(deftest x (testing \"t\" (fires 1 1)))")))
    (is (hits tests/empty-test "(defn- noop [a] a)\n(deftest x (noop 1))"))))

(deftest testing-without-assertion
  (fires tests/testing-without-assertion
         "(deftest t (is (= 1 1)) (testing \"nothing\" (println :x)))"
         "(deftest t (testing \"ok\" (is (= 1 1))))"))

(deftest test-with-no-effect
  (fires tests/test-with-no-effect
         "(deftest t (is (= 1)))"
         "(deftest t (is (= 1 1)))"))

(deftest jndi-injection-both-forms
  (let [table {:classes {"javax.naming.InitialContext" {:supers ["Context" "Object"] :ctors [{:params []}] :methods {"lookup" [{:params ["String"] :returns "Object"} {:params ["Name"] :returns "Object"}] "doLookup" [{:params ["String"] :returns "Object" :static? true}]}}
                         "javax.naming.Context" {:supers ["Object"] :methods {"lookup" [{:params ["String"] :returns "Object"} {:params ["Name"] :returns "Object"}]}}}
               :by-simple {"InitialContext" ["javax.naming.InitialContext"] "Context" ["javax.naming.Context"]}}
        l (sift/linter {:oracle {:tags table}})
        ids (fn [src] (set (map :rule (:findings (sift/lint l [{:path "x.clj" :text src}])))))]
    (is (contains? (ids "(ns x (:import [javax.naming InitialContext]))\n(defn look [n] (InitialContext/doLookup n))") :security/jndi-injection) "static form, class through the import")
    (is (not (contains? (ids "(ns x (:import [javax.naming InitialContext]))\n(defn look [] (InitialContext/doLookup \"java:comp/env\"))") :security/jndi-injection)) "a literal name")
    (is (contains? (ids "(ns x (:import [javax.naming InitialContext]))\n(defn look [n] (.lookup (InitialContext.) n))") :security/jndi-injection) "instance form on a constructed context")
    (is (contains? (ids "(ns x (:import [javax.naming Context]))\n(defn look [^Context ctx n] (.lookup ctx n))") :security/jndi-injection) "instance form on a hinted receiver")
    (is (not (contains? (ids "(defn look [ctx n] (.lookup ctx n))") :security/jndi-injection)) "an untyped receiver: no claim, not a guess")))

(deftest interop-detections-as-patterns
  (let [l (sift/linter {})
        ids (fn [src] (set (map :rule (:findings (sift/lint l [{:path "x.clj" :text src}])))))]
    (is (contains? (ids "(ns x (:import [java.io ObjectInputStream]))\n(defn r [in] (ObjectInputStream. in))") :security/unsafe-deserialization))
    (is (contains? (ids "(defn t [] (java.io.File/createTempFile \"a\" \"b\"))") :security/predictable-temp-file))
    (is (contains? (ids "(defn p [c] (ProcessBuilder. c))") :security/process-spawn) "java.lang needs no import")
    (is (not (contains? (ids "(defn p [c] (str c))") :security/process-spawn)))))

(deftest none-of-them-throw-on-unparseable-or-empty-input
  (doseq [f [#(tenancy/ambiguous-owner-check % tenant) #(tenancy/unscoped-tenant-query % tenant shared)
             web/xss-unescaped-output web/csrf-protection-absent web/sensitive-data-logged web/cookie-missing-security-flags
             regex/redos-vulnerable-regex regex/partial-match-validation
             tests/empty-test tests/testing-without-assertion tests/test-with-no-effect
             interop/trust-all-certificates interop/xml-external-entity
             concurrency/side-effect-in-swap concurrency/discarded-future
             security/shell-invocation security/hardcoded-credential security/xml-parse security/permissive-file-permissions]
          src ["" "(" ")" "#_" ";; just a comment" "(defn"]]
    (is (nil? (try (doall (f (:nodes (parse/parse src)))) nil
                   (catch Throwable t t)))
        (str "threw on " (pr-str src)))))
