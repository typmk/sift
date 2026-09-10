(ns com.typemark.sift.data-ancestors-test
  "The ancestor walk that `:inside` and `:not-inside` are built on. Two
  defects lived in it until 2026-09-10:

    #(…) is ONE :fn node with no list node for its body, so the walk skipped
    exactly one level inside every anonymous function — see the corpus pair
    swap_in_anon_fn, which the shipped :deref-inside-own-swap now catches.

    a ^{…} :meta node's sexpr IS its child's, so an annotated form was its
    own ancestor. NOTHING SHIPPED CAN SEE THAT: no rule in rules.edn is
    self-referential, and `findings` only walks inside a defining form, so
    the (defonce x ^{:doc …} (atom {})) that showed it is never visited. It
    is gated here with such a rule defined locally, because the first
    self-referential rule to land would otherwise inherit the defect."
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.data :as data]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.zip :as z]))

(def ^:private nested-reference
  [{:id :nested-reference-probe
    :kind :forbid
    :either '[(atom ?&_) (ref ?&_)]
    :inside '(atom ?&_)
    :category :design
    :applicability :unspecified
    :message "a reference type inside another"
    :instruction "Hold one value in one atom."}])

(defn- probe [src]
  (with-redefs [data/rules nested-reference]
    (mapv :rule (data/findings "probe.clj"
                               (z/edn* (parser/parse-string-all src)
                                       {:track-position? true})))))

(deftest a-meta-node-is-not-an-ancestor
  (testing "an annotated construction is not a construction inside itself"
    (is (empty? (probe "(defn f [] (let [x ^{:doc \"d\"} (atom 1)] x))")))
    (is (empty? (probe "(defn f [] ^:private (atom 1))"))))
  (testing "and a real nesting still fires"
    (is (= [:nested-reference-probe]
           (probe "(defn f [] (atom {:history (atom [])}))")))))

(deftest an-anon-fn-body-is-walked-into
  (testing "#(…) is one :fn node with no list node for its body"
    (is (= [:nested-reference-probe]
           (probe "(defn f [] (map #(atom {:inner (atom 1)}) [1]))")))))

(deftest the-top-level-gate-is-what-hid-the-meta-defect
  (testing "findings walks only inside a defining form, so a top-level
            annotated def is never visited at all — recorded so the next
            reader does not mistake silence here for the fix working"
    (is (empty? (probe "(defonce registry ^{:doc \"static\"} (atom {}))")))
    (is (empty? (probe "(def s (atom {:history (atom [])}))")))))
