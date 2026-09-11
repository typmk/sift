(ns com.typemark.sift.tree-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.parse :as parse]
            [com.typemark.sift.tree :as tree]))

(def ^:private sources
  "Every node shape whose span could disagree with its place in the tree:
  metadata, reader macros, #(), quotes, #_, (comment), derefs, namespaced
  maps, a multi-line string, and siblings sharing a line."
  ["(defn f [x] (let [a (atom 0)] (swap! a inc) (if x (swap! a assoc :k x) (swap! a dissoc :k))))"
   "^:m (def ^{:doc \"d\"} y #(inc %)) '(a b) `(c ~d) @x #'v #?(:clj 1 :cljs 2) #?@(:clj [3])"
   "#_(dead (thing)) (comment (x) (y)) #:ns{:a 1 :b [2 3]} #{1 2} {:k (f) :j [g]}"
   "(ns a (:require [b :as c]))\n(defn g\n  \"doc\n  on two lines\"\n  [] (c/h) (i))\n(j) (k (l) (m))"])

(deftest the-slice-and-the-span-scan-agree
  (testing "parse's own vector is sliced; any other stream is scanned by span"
    (doseq [s (conj sources (slurp "src/com/typemark/sift/tree.cljc"))
            :let [nodes (:nodes (parse/parse s))]
            n nodes
            :when (not= :forms (:tag n))]
      (is (= (vec (tree/children-of nodes n)) (vec (tree/children-of (seq nodes) n))) (:text n))
      (is (= (tree/parent nodes n) (tree/parent (seq nodes) n)) (:text n)))))

(deftest a-sibling-on-the-same-line-is-not-a-child
  (let [nodes (:nodes (parse/parse "(a (b) (c))"))
        b (first (filter #(= "(b)" (:text %)) nodes))]
    (is (= ["b"] (map :text (tree/children-of nodes b))))
    (is (= ["b"] (map :text (tree/children-of (seq nodes) b))))))
