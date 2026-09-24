(ns net.typemark.sift.node-rules-test
  "A flag/clear pair per node rule — the corpus gate the shape and typeflow
  families already have, for the families that predate them. A positive
  alone proves a rule can fire, which a rule matching everything also does —
  so every rule gets both, and the :corpus rung for these rules means THIS
  file."
  (:require [clojure.test :refer [deftest is testing]]
            [net.typemark.sift :as sift]
            [net.typemark.sift.access :as access]
            [net.typemark.sift.interop :as interop]
            [net.typemark.sift.parse :as parse]
            [net.typemark.sift.regex :as regex]
            [net.typemark.sift.tests :as tests]
            [net.typemark.sift.web :as web]))

(defn- rules [f src] (set (map :rule (f (:nodes (parse/parse src))))))

(defn- fires
  "Asserts `rule` is raised for `bad` and not for `good` -- the pair, always."
  [f rule bad good]
  (is (contains? (rules f bad) rule) (str rule " missed: " (pr-str bad)))
  (is (not (contains? (rules f good) rule))
      (str rule " false positive on: " (pr-str good))))

;; ---------------------------------------------------------------- access

(deftest ambiguous-owner-check
  (testing "nil owner means both untenanted and unresolved, and a two-valued test sends both down one branch"
    (fires access/findings "ambiguous-owner-check"
           "(defn h [o] (if (nil? (:obj/owner o)) (throw (ex-info \"no\" {})) :ok))"
           "(defn h [o] (case (owner-of o) ::unresolved (throw (ex-info \"no\" {})) :ok))")))

(deftest unscoped-tenant-query
  (fires access/findings "unscoped-tenant-query"
         "(d/q '[:find ?e :where [?e :product/sku]] db)"
         "(d/q '[:find ?e :in $ ?owner :where [?e :obj/owner ?owner]] db o)")
  (testing "a pull takes an entity id the caller already holds — 40 of 124 findings on three services, not one a scan"
    (is (empty? (rules access/findings "(d/pull db '[*] eid)"))))
  (testing "the schema has no tenant to name"
    (is (empty? (rules access/findings "(d/q '[:find ?e :where [?e :db/ident _]] db)"))))
  (testing "nor does a project's shared layer, once the caller names it"
    (let [q "(d/q '[:find ?e :where [?e :taxon/factor _]] db)"]
      (is (contains? (rules access/findings q) "unscoped-tenant-query"))
      (is (empty? (set (map :rule (access/findings (:nodes (parse/parse q)) :shared-ns #{:taxon})))))))
  (testing "a query naming no attribute says nothing either way; the safer reading keeps it"
    (is (contains? (rules access/findings "(d/q query db)") "unscoped-tenant-query")))
  (testing "a corpus names a tenant when any token does, wherever it sits in the text"
    (is (sift/tenanted? ["(d/q '[:find ?e :in $ ?o :where [?e :obj/owner ?o]] db o)"]))
    (is (sift/tenanted? ["(defn f [tenant-id] tenant-id)"]))
    (is (not (sift/tenanted? ["(def ownership 1)" "(defn organise [x] x)"]))
        "a longer identifier is not a tenant key"))
  (testing "vacuous over a corpus that never names a tenant — :tenanted? false, from sift/tenanted?"
    (is (empty? (set (map :rule (access/findings (:nodes (parse/parse "(d/q query db)")) :tenanted? false)))))))

;; ----------------------------------------------------------------- regex

(deftest redos-vulnerable-regex
  (testing "brace-nested quantifiers, the shape that actually backtracks"
    (fires regex/findings "redos-vulnerable-regex"
           "(def p #\"(a{1,9}){1,9}\")"
           "(def p #\"^[a-z]+$\")")))

(deftest partial-match-validation
  (testing "re-find succeeds on a PARTIAL match; an unanchored pattern in a decision position accepts anything containing one"
    (fires regex/findings "partial-match-validation"
           "(defn ok? [s] (when (re-find #\"good\\\\.example\" s) :yes))"
           "(defn ok? [s] (when (re-find #\"^good$\" s) :yes))")))

;; ------------------------------------------------------------------- web

(deftest xss-unescaped-output
  (fires web/findings "xss-unescaped-output"
         "(h/raw (str \"<b>\" x \"</b>\"))"
         "(h/raw \"<hr>\")"))

(deftest csrf-protection-absent
  (fires web/findings "csrf-protection-absent"
         "(ns app.routes (:require [reitit.ring :as ring]))\n(def routes [[\"/pay\" {:post handler}]])"
         (str "(ns app.routes (:require [reitit.ring :as ring]"
              " [ring.middleware.anti-forgery :as af]))\n"
              "(def routes [[\"/pay\" {:post handler}]])"))
  (testing "a method keyword as an argument or a dispatch-table key is not a route"
    (is (empty? (rules web/findings "(ns app.routes (:require [reitit.ring :as ring]))\n(defn f [db] (crud-decision db :delete 1))")))
    (is (empty? (rules web/findings "(ns app.routes (:require [reitit.ring :as ring]))\n(def ops {:delete (partial delete-op conn)})")))))

(deftest sensitive-data-logged
  (fires web/findings "sensitive-data-logged"
         "(log/info \"tok\" auth-token)"
         "(log/info \"count\" n)"))

(deftest cookie-missing-security-flags
  (fires web/findings "cookie-missing-security-flags"
         "(def c {:cookies {\"sid\" {:value v :max-age 3600}}})"
         "(def c {:cookies {\"sid\" {:value v :max-age 3600 :http-only true :secure true}}})"))

;; ----------------------------------------------------------------- tests

(deftest empty-test
  (fires tests/findings "empty-test"
         "(deftest nothing (println :hi))"
         "(deftest something (is (= 1 1)))")
  (testing "a deftest delegating to a helper that asserts is not empty; one delegating to a helper that does not, is"
    (is (empty? (rules tests/findings "(defn- fires [a b] (is (= a b)))\n(deftest x (testing \"t\" (fires 1 1)))")))
    (is (contains? (rules tests/findings "(defn- noop [a] a)\n(deftest x (noop 1))") "empty-test"))))

(deftest testing-without-assertion
  (fires tests/findings "testing-without-assertion"
         "(deftest t (is (= 1 1)) (testing \"nothing\" (println :x)))"
         "(deftest t (testing \"ok\" (is (= 1 1))))"))

(deftest test-with-no-effect
  (fires tests/findings "test-with-no-effect"
         "(deftest t (is (= 1)))"
         "(deftest t (is (= 1 1)))"))

;; --------------------------------------------------------------- interop

(deftest jndi-injection-both-forms
  ;; rules.edn now; the instance form needs the receiver's type, which the
  ;; data engine reads from typeflow's annotated walk and the oracle's table
  (let [table {:classes {"javax.naming.InitialContext" {:supers ["Context" "Object"] :ctors [{:params []}] :methods {"lookup" [{:params ["String"] :returns "Object"} {:params ["Name"] :returns "Object"}] "doLookup" [{:params ["String"] :returns "Object" :static? true}]}}
                         "javax.naming.Context" {:supers ["Object"] :methods {"lookup" [{:params ["String"] :returns "Object"} {:params ["Name"] :returns "Object"}]}}}
               :by-simple {"InitialContext" ["javax.naming.InitialContext"] "Context" ["javax.naming.Context"]}}
        ids (fn [src] (set (map :rule (:findings (sift/analyze {:text src :path "x.clj" :classes table})))))]
    (is (contains? (ids "(ns x (:import [javax.naming InitialContext]))\n(defn look [n] (InitialContext/doLookup n))") :jndi-injection) "static form, class through the import")
    (is (not (contains? (ids "(ns x (:import [javax.naming InitialContext]))\n(defn look [] (InitialContext/doLookup \"java:comp/env\"))") :jndi-injection)) "a literal name")
    (is (contains? (ids "(ns x (:import [javax.naming InitialContext]))\n(defn look [n] (.lookup (InitialContext.) n))") :jndi-injection) "instance form on a constructed context")
    (is (contains? (ids "(ns x (:import [javax.naming Context]))\n(defn look [^Context ctx n] (.lookup ctx n))") :jndi-injection) "instance form on a hinted receiver")
    (is (not (contains? (ids "(defn look [ctx n] (.lookup ctx n))") :jndi-injection)) "an untyped receiver: no claim, not a guess")))

(deftest interop-detections-that-became-data
  (let [ids (fn [src] (set (map :rule (:findings (sift/analyze {:text src :path "x.clj"})))))]
    (is (contains? (ids "(ns x (:import [java.io ObjectInputStream]))\n(defn r [in] (ObjectInputStream. in))") :unsafe-deserialization))
    (is (contains? (ids "(defn t [] (java.io.File/createTempFile \"a\" \"b\"))") :predictable-temp-file))
    (is (contains? (ids "(defn p [c] (ProcessBuilder. c))") :shell-invocation) "java.lang needs no import")
    (is (not (contains? (ids "(defn p [c] (str c))") :shell-invocation)))))

;; xml-external-entity's pair — parser built, hardening set or not — is in
;; interop_test; banned-term's is in dictionary_test; the credential,
;; permissions, shell, side-effect-in-swap and discarded-future pairs are in
;; security_test. One gate, several files.

;; ------------------------------------------------------------ invariants

(deftest none-of-them-throw-on-unparseable-or-empty-input
  (doseq [f [access/findings web/findings regex/findings tests/findings interop/all-findings]
          src ["" "(" ")" "#_" ";; just a comment" "(defn"]]
    (is (nil? (try (doall (f (:nodes (parse/parse src)))) nil
                   (catch Throwable t t)))
        (str "threw on " (pr-str src)))))
