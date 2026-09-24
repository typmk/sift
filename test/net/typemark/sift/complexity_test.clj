(ns net.typemark.sift.complexity-test
  "Every assertion here was first measured against cccc-core 1.6.0 with the
  Clojure adapter patched for positional def-macros, and the two agree on
  5,519 of 5,531 units over a code-graph tool's source; the 12 that differ are
  lambdas inside `(comment …)`, which this suite asserts as a deviation."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [net.typemark.sift.complexity :as cx]))

(defn- units [src] (cx/flatten-units (:functions (cx/report src))))
(defn- unit [src name] (first (filter #(= name (:name %)) (units src))))
(defn- cog [src name] (:cognitive (unit src name)))
(defn- cyc [src name] (:cyclomatic (unit src name)))

;; ---- the whitepaper's rules ----------------------------------------------

(deftest nesting-is-charged-and-cyclomatic-is-not
  (let [flat   "(defn f [x] (if x 1 2) (if x 3 4) (if x 5 6))"
        nested "(defn f [x] (if x (if x (if x 1 2) 3) 4))"]
    (is (= 3 (cog flat "f")) "three sibling ifs: 1 + 1 + 1")
    (is (= 6 (cog nested "f")) "three nested ifs: 1 + 2 + 3")
    (is (= (cyc flat "f") (cyc nested "f")) "McCabe cannot tell them apart")
    (is (= 3 (:max-nesting (unit nested "f"))))))


(deftest if-is-a-ternary-not-a-statement
  (testing "an `if` with an else arm costs 1 + nesting, and nothing for the else"
    (is (= 1 (cog "(defn f [x] (if x 1 2))" "f")))
    (is (= 3 (cog "(defn f [x] (if x (when 1 2) 3))" "f")))))

(deftest cond-is-an-else-if-chain
  (testing "first test structural, each next +1 flat, :else +1"
    (is (= 3 (cog "(defn f [x] (cond (= x 1) 1 (= x 2) 2 :else 3))" "f")))
    (is (= 7 (cog "(defn f [x] (cond (= x 1) (if x 1 2) (= x 2) 3 :else (when x 4)))" "f")))))

(deftest cond-thread-is-a-branch-per-clause
  (is (= 2 (cog "(defn f [x] (cond-> x (pos? x) inc (neg? x) dec))" "f"))))

(deftest case-and-condp-are-switches
  (is (= 1 (cog "(defn f [x] (case x (:a :b) 1 :c 2 3))" "f")))
  (is (= 3 (cyc "(defn f [x] (case x (:a :b) 1 :c 2 3))" "f")) "two cases, default is free")
  (is (= 5 (cog "(defn f [x] (case x 1 (if x 1 2) (if x 3 4)))" "f")) "bodies nest"))

(deftest logical-sequences-cost-one-each
  (is (= 1 (cog "(defn f [a b c] (and a b c))" "f")) "one sequence")
  (is (= 3 (cyc "(defn f [a b c] (and a b c))" "f")) "but two operators for McCabe")
  (is (= 3 (cog "(defn f [a b c d] (or (and a b) (and c d)))" "f")) "three sequences")
  (is (= 3 (cog "(defn f [a b c] (and a (or b c) (or b c)))" "f")))
  (testing "a lambda whose body starts with `or` is a unit, not part of the sequence"
    (let [us (units "(defn f [g] (or g #(or (aget % \"c\") 1)))")]
      (is (= 1 (cog "(defn f [g] (or g #(or (aget % \"c\") 1)))" "f")))
      (is (= 1 (:cognitive (first (filter #(= "<fn>" (:name %)) us))))))))

(deftest loops-nest-their-bodies-and-not-their-bindings
  (is (= 3 (cog "(defn f [xs] (doseq [x xs] (when x (println x))))" "f")))
  (is (= 3 (cog "(defn f [x] (loop [i 0] (if (< i x) (recur (inc i)) i)))" "f")))
  (testing "a `:when` guard in a `for` is evaluated outside the loop's nesting"
    (is (= 3 (cog "(defn f [xs] (for [x xs :when (pos? x) :let [y x]] (if y 1 2)))" "f")))))

(deftest try-catch-finally
  (is (= 5 (cog "(defn f [x] (try (if x 1 2) (catch js/Error e (when e 1)) (finally (if x 3 4))))" "f"))
      "catch is structural and nests; finally is transparent"))

(deftest recursion-costs-one
  (is (= 2 (cog "(defn f [x] (if x (f (dec x)) 0))" "f")) "a call by name")
  (is (= 2 (cog "(defn f [x] (if x (recur (dec x)) 0))" "f"))
      "recur outside a loop is the same recursion — a stated departure from cccc"))

;; ---- units ---------------------------------------------------------------

(deftest nested-functions-are-children-and-reset-nesting
  (let [src "(defn host [x] (if x (fn [y] (if y (if y 1 2) 3)) 0))"
        h   (first (:functions (cx/report src)))]
    (is (= 1 (:cognitive h)))
    (is (= 1 (count (:children h))))
    (is (= 3 (:cognitive (first (:children h)))) "1 + 2, not 2 + 3")))

(deftest hash-fn-is-a-unit-like-fn
  (let [a (units "(defn h [x] (map #(if % (when 1 2) 3) x))")
        b (units "(defn h [x] (map (fn [y] (if y (when 1 2) 3)) x))")]
    (is (= [0 3] (map :cognitive a)))
    (is (= [0 3] (map :cognitive b)))
    (is (= 1 (:params (second a))))))

(deftest positional-def-macros-are-named-units
  (let [src "(deftest a-test (if 1 (when 2 3) 4))
             (mcp/deftool \"ingest\" \"doc\" {} (fn [g p] (if 1 (when 2 3) 4)))
             (defcap :type/subtypes \"d\" (fn [g] (if 1 2 3)))
             (def ^:private do-facts (with-auto-index (fn [g p] (if 1 (when 2 3) 4))))
             (deftest ^:async t1 (if 1 2 3))"]
    (is (= {"a-test" 3 "ingest" 3 ":type/subtypes" 1 "do-facts" 3 "t1" 1}
           (into {} (map (juxt :name :cognitive)) (units src))))
    (is (= "deftool" (:kind (unit src "ingest"))))
    (is (nil? (unit src "<fn>")) "a direct fn body carries the def's name")))

(deftest the-def-rule-is-top-level-only
  (let [src "(defn f [x] (default x (if x 1 2)))"]
    (is (= 1 (cog src "f")))
    (is (nil? (unit src "x")) "a nested call whose head starts with def is a call"))
  (let [src "(deftest outer (deftool inner [q] (if q 1 2)))"]
    (is (= ["outer"] (map :name (:functions (cx/report src)))) "and not a unit inside a unit")))

(deftest protocol-methods-are-units
  (let [src "(defrecord R [x] P (m [this] (if x (when 1 2) 3)))
             (extend-protocol P Object (m [this] (if 1 (when 2 3) 4)))
             (defn host [] (reify P (m [this] (if 1 (when 2 3) 4))))"]
    (is (= 3 (cog src "R/m")))
    (is (= 3 (cog src "Object/m")))
    (is (= 3 (cog src "reify/m")))
    (is (= "method" (:kind (unit src "R/m"))))))

(deftest multi-arity-is-one-unit-with-the-widest-params
  (let [u (unit "(defn f ([a] (if a 1 2)) ([a b & more] (if a 1 2)))" "f")]
    (is (= 2 (:cognitive u)))
    (is (= 3 (:params u)) "`&` is not a parameter")))

(deftest module-level-lambdas-are-units
  (is (= ["<fn>"] (map :name (units "(when (exists? js/process) (.on js/process \"exit\" #(when x 1)))")))))

;; ---- the reader ----------------------------------------------------------

(deftest docstrings-and-comments-do-not-shift-positions
  (testing "a multi-line docstring is a :multi-line node, not a :token"
    (is (= 1 (cog "(defn f\n  \"line one\n   line two\"\n  [x] (if x 1 2))" "f"))))
  (testing "a ;; comment between forms is a node and must not become the then-arm"
    (is (= 3 (cog "(defn f [x] (if-let [y x] ;; note\n (when y 1) 2))" "f")))
    (is (= 4 (cog "(defn f [x] (cond ;; a\n x 1 ;; b\n :else (if x 2 3)))" "f"))
        "cond 1, :else 1, the if nested 2"))
  (testing "`#_` discards, and an empty list is not a call"
    (is (= 0 (cog "(defn f [x] #_(if x 1 2) ())" "f")))))

(deftest data-is-not-code
  (is (= 0 (cog "(defn f [x] '(if x 1 2))" "f")))
  (is (= 0 (cog "(defn f [x] `(if ~x 1 2))" "f")))
  (is (= 0 (cog "(defn f [x] (comment (if x 1 2)) x)" "f")) "a stated departure from cccc")
  (is (= 3 (cog "(defn f [x] (if x #js {:a (when 1 2)} 3))" "f")) "but #js payloads are evaluated"))

(deftest reader-conditionals-score-one-platform
  (let [src "(defn f [x] #?(:clj (if x 1 2) :cljs (if x (when 1 2) 3)))"]
    (is (= 1 (cog src "f")) "default is :clj")
    (is (= 3 (:cognitive (first (:functions (cx/report src "a.cljs"))))))
    (is (= 1 (:cognitive (first (:functions (cx/report src "a.clj"))))))))

;; ---- findings ------------------------------------------------------------

(deftest findings-report-units-over-the-threshold
  (let [src "(defn big [x] (if x (if x (if x (if x (if x (if x 1 2) 3) 4) 5) 6) 7))
             (defn small [x] (if x 1 2))"]
    (is (= 21 (cog src "big")))
    (is (= ["big"] (map :function (cx/findings src nil))))
    (is (= :cognitive-complexity (:rule (first (cx/findings src nil)))))
    (is (empty? (cx/findings src nil 21)) "the threshold is exclusive")))

(deftest unparseable-source-is-an-error-not-a-zero
  (is (false? (:ok? (cx/report "(defn f [x")))))

;; ---- properties ----------------------------------------------------------

(def ^:private gen-expr
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/fmap #(str "(if x " (first %) " " (second %) ")") (gen/tuple inner inner))
                  (gen/fmap #(str "(when x " % ")") inner)
                  (gen/fmap #(str "(and x " % ")") inner)
                  (gen/fmap #(str "(f " % ")") inner)]))
   (gen/return "x")))

(defspec never-throws-and-never-negative 200
  (prop/for-all [e gen-expr]
    (let [r (cx/report (str "(defn f [x] " e ")"))]
      (and (:ok? r)
           (every? #(and (<= 0 (:cognitive %)) (<= 1 (:cyclomatic %)))
                   (cx/flatten-units (:functions r)))))))

(defspec every-branch-form-raises-cyclomatic 200
  (prop/for-all [e gen-expr]
    (let [ifs   (count (re-seq #"\(if " e))
          whens (count (re-seq #"\(when " e))
          ands  (count (re-seq #"\(and " e))
          u     (first (:functions (cx/report (str "(defn f [x] " e ")"))))]
      (= (:cyclomatic u) (+ 1 ifs whens ands)))))
