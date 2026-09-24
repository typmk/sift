(ns net.typemark.sift.tree-test
  (:require [clojure.test :refer [deftest is testing]]
            [net.typemark.sift.parse :as parse]
            [net.typemark.sift.tree :as tree]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

(defn- node-strings
  [s]
  (letfn [(walk [node]
            (cons (when (:row (meta node)) (n/string node))
                  (when (n/inner? node) (mapcat walk (n/children node)))))]
    (remove nil? (walk (p/parse-string-all s)))))

(deftest text-is-what-every-node-read-as
  (doseq [s ["(a\r\n  (b \"é\")\r\n  c)" "#?(:clj [1 2] :cljs {:k #(inc %)})" "^:private (def x 1)"
             (slurp "src/net/typemark/sift/tree.cljc")]
          :let [nodes (:nodes (parse/parse s))]]
    (is (= (node-strings s) (map #(tree/text nodes %) nodes)) s)
    (testing "an inner node carries none of its own"
      (is (every? (comp nil? :text) (filter :inner? nodes))))))

(def ^:private sources
  ["(defn f [x] (let [a (atom 0)] (swap! a inc) (if x (swap! a assoc :k x) (swap! a dissoc :k))))"
   "^:m (def ^{:doc \"d\"} y #(inc %)) '(a b) `(c ~d) @x #'v #?(:clj 1 :cljs 2) #?@(:clj [3])"
   "#_(dead (thing)) (comment (x) (y)) #:ns{:a 1 :b [2 3]} #{1 2} {:k (f) :j [g]}"
   "(ns a (:require [b :as c]))\n(defn g\n  \"doc\n  on two lines\"\n  [] (c/h) (i))\n(j) (k (l) (m))"])

(deftest the-slice-and-the-span-scan-agree
  (testing "parse's own vector is sliced; any other stream is scanned by span"
    (doseq [s (conj sources (slurp "src/net/typemark/sift/tree.cljc"))
            :let [nodes (:nodes (parse/parse s))]
            n nodes
            :when (not= :forms (:tag n))]
      (is (= (vec (tree/children-of nodes n)) (vec (tree/children-of (seq nodes) n))) (:text n))
      (is (= (tree/parent nodes n) (tree/parent (seq nodes) n)) (:text n)))))

(deftest a-sibling-on-the-same-line-is-not-a-child
  (let [nodes (:nodes (parse/parse "(a (b) (c))"))
        b (first (filter #(= "(b)" (tree/text nodes %)) nodes))]
    (is (= ["b"] (map :text (tree/children-of nodes b))))
    (is (= ["b"] (map :text (tree/children-of (seq nodes) b))))))
