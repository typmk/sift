(ns com.typemark.sift.parse-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.typemark.sift.parse :as parse]))

(defn- nodes [s] (:nodes (parse/parse s)))
(defn- leaf-types [s] (mapv :type (remove #(= :trivia (:type %)) (parse/leaves (nodes s)))))

(deftest classifies-the-token-kinds-sonar-colours-differently
  (is (= [:symbol :number] (leaf-types "(inc 1)")))
  (is (= [:string] (leaf-types "\"hello\"")))
  (is (= [:comment] (leaf-types "; a comment")))
  (is (= [:keyword] (leaf-types ":foo/bar")))
  (is (= [:keyword] (leaf-types "::auto")))
  (is (= [:regex] (leaf-types "#\"[a-z]+\"")))
  (is (= [:char] (leaf-types "\\newline")))
  (is (= [:number] (leaf-types "-42")))
  (testing "a semicolon inside a string is not a comment"
    (is (= [:string] (leaf-types "\"; not a comment\""))))
  (testing "a character literal of an open paren does not open a form"
    (is (= [:char] (leaf-types "\\(")))))

(deftest reader-conditionals-parse-as-syntax-not-as-a-failure
  (let [r (parse/parse "#?(:clj 1 :cljs 2)")]
    (is (:ok? r))
    (is (some #(= :reader-macro (:tag %)) (:nodes r)))))

(deftest commented-out-code-is-marked-as-such
  (testing "#_ marks its whole subtree"
    (let [ns' (nodes "#_(dead thing)")]
      (is (every? :commented? (remove #(= :forms (:tag %)) ns')))))
  (testing "(comment ...) marks its body"
    (let [ns' (nodes "(comment (inc 1))")]
      (is (some #(and (= :list (:tag %)) (:commented? %)) ns'))))
  (testing "ordinary code is not marked"
    (is (not-any? :commented? (nodes "(inc 1)")))))

(deftest head-position-is-a-tree-fact
  (testing "a binding named like a branch is not a branch"
    (is (not-any? :branch? (nodes "(let [and 1] and)"))))
  (testing "a real branch is"
    (is (some :branch? (nodes "(and a b)"))))
  (testing "nesting depth is tracked for cognitive complexity"
    (let [inner (->> (nodes "(if a (if b 1 2) 3)") (filter :branch?) (map :branch-nesting) sort)]
      (is (= [0 1] inner)))))

(deftest literals-collapse-for-duplication-matching
  (is (= "$STRING" (parse/cpd-image {:type :string :text "\"anything\""})))
  (is (= "$NUMBER" (parse/cpd-image {:type :number :text "42"})))
  (is (= "inc" (parse/cpd-image {:type :symbol :text "inc"})))
  (testing "two blocks differing only in constants match"
    (let [img #(mapv parse/cpd-image (parse/leaves (nodes %)))]
      (is (= (img "(+ 1 2)") (img "(+ 9 8)"))))))

(deftest unbalanced-source-fails-loudly-rather-than-silently
  (let [r (parse/parse "(defn f [")]
    (is (false? (:ok? r)))
    (is (string? (:error r)) "the reason reaches the scanner log")))

(defspec never-throws-on-arbitrary-input 400
  (prop/for-all [s gen/string]
    (contains? (parse/parse s) :ok?)))

(defspec never-throws-on-clojure-shaped-input 400
  (prop/for-all [s (gen/fmap #(apply str %)
                             (gen/vector (gen/elements ["(" ")" "[" "]" "{" "}" "\"" "\\"
                                                        "#" ";" ":" "'" "~" "@" "^" "`"
                                                        "a" " " "\n" "1" "," "/" "*" "#_"])
                                         0 60))]
    (contains? (parse/parse s) :ok?)))

(defspec every-node-has-a-sane-span 300
  (prop/for-all [s gen/string-alphanumeric]
    (every? (fn [t] (and (>= (:line t) 1)
                         (>= (:col t) 1)
                         (>= (:end-line t) (:line t))))
            (nodes s))))
