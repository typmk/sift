(ns au.com.heisenbergtech.scan.callgraph-test
  (:require [clojure.test :refer [deftest is testing]]
            [au.com.heisenbergtech.scan.callgraph :as cg]))

(def analysis
  (str "{\"analysis\":{\"var-usages\":["
       "{\"filename\":\"p.clj\",\"from\":\"probe\",\"from-var\":\"handler\","
       "\"to\":\"probe\",\"name\":\"danger\","
       "\"name-row\":4,\"name-col\":22,\"name-end-row\":4,\"name-end-col\":28},"
       "{\"filename\":\"p.clj\",\"from\":\"probe\",\"from-var\":\"handler\","
       "\"to\":\"probe\",\"name\":\"untrusted\","
       "\"name-row\":4,\"name-col\":30,\"name-end-row\":4,\"name-end-col\":39},"
       "{\"filename\":\"p.clj\",\"from\":\"probe\",\"from-var\":\"innocent\","
       "\"to\":\"clojure.core\",\"name\":\"+\","
       "\"name-row\":5,\"name-col\":22,\"name-end-row\":5,\"name-end-col\":23}"
       "]}}"))

(def seeds {:taints #{["probe" "untrusted"]} :reaches #{["probe" "danger"]}})

(deftest builds-a-call-graph-from-from-var
  (let [g (cg/call-graph analysis)]
    (is (= #{["probe" "danger"] ["probe" "untrusted"]}
           (set (map :callee (get g ["probe" "handler"])))))
    (is (= #{["clojure.core" "+"]}
           (set (map :callee (get g ["probe" "innocent"])))))))

(deftest propagation-reaches-a-fixpoint
  (let [{:keys [taints reaches paths]} (cg/propagate (cg/call-graph analysis) seeds)]
    (testing "a caller of a tainting var taints"
      (is (contains? taints ["probe" "handler"])))
    (testing "a caller of a reaching var reaches"
      (is (contains? reaches ["probe" "handler"])))
    (testing "the path is where the two meet"
      (is (= #{["probe" "handler"]} paths)))
    (testing "an unrelated var is on neither side"
      (is (not (contains? taints ["probe" "innocent"])))
      (is (not (contains? reaches ["probe" "innocent"]))))))

(deftest reports-the-crossing-with-a-flow
  (let [fs (cg/findings analysis seeds)]
    (is (= 1 (count fs)))
    (let [f (first fs)]
      (is (= "interprocedural-taint" (:rule f)))
      (is (= "p.clj" (:filename f)))
      (is (re-find #"probe/handler" (:message f)))
      (testing "the flow shows both ends of the path"
        (is (= 2 (count (:flow f))))))))

(deftest a-var-that-is-both-source-and-sink-is-left-to-the-direct-pass
  (testing "reporting it here too would double-count the same defect"
    (let [both {:taints #{["probe" "handler"]} :reaches #{["probe" "handler"]}}]
      (is (empty? (cg/findings analysis both))))))

(deftest no-seeds-means-no-findings
  (is (empty? (cg/findings analysis {:taints #{} :reaches #{}}))))
